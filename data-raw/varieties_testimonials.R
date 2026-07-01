## Reproducible extraction of first-person testimonials from
## William James (1902), "The Varieties of Religious Experience."
##
## Source: Project Gutenberg, https://www.gutenberg.org/ebooks/621
## License: Public domain (published 1902).
##
## This script downloads the full text and extracts the indented
## first-person testimonial blocks that James quotes throughout the
## lectures. Run once to regenerate inst/extdata/varieties_testimonials.csv.
##
## Requires: curl (system), Python 3

library(curl)

dest <- tempfile(fileext = ".txt")
curl_download("https://www.gutenberg.org/files/621/621-0.txt", dest)

py_script <- r"(
import re, csv, sys

with open(sys.argv[1], 'r', encoding='utf-8') as f:
    lines = f.readlines()

blocks = []
current_lines = []
start_line = None

for i, line in enumerate(lines):
    stripped = line.rstrip('\n')
    if stripped.startswith('    '):
        if start_line is None:
            start_line = i + 1
        current_lines.append(stripped.strip())
    else:
        if current_lines:
            text = ' '.join(current_lines)
            text = re.sub(r'‐\s*', '', text)
            text = re.sub(r'\s+', ' ', text).strip()
            blocks.append({'start_line': start_line, 'word_count': len(text.split()), 'text': text})
            current_lines = []
            start_line = None

fp = re.compile(r'\bI\b|\bmy\b|\bme\b|\bmyself\b', re.IGNORECASE)
candidates = [b for b in blocks if 200 <= b['word_count'] <= 1100 and fp.search(b['text'])]

# Manually curated selection of psychologically rich testimonials
selected_lines = [
    1814, 1997, 2067, 2088, 2114, 3251, 3848, 3997, 4041,
    4544, 4861, 4917, 4951, 5177, 5296, 5336, 5370, 5478,
    5891, 5927, 6189, 6209, 6504, 6528, 6659, 6769, 6818,
    6929, 7443, 7479, 7652, 7784, 8344, 8854, 9398, 11611,
    11738, 11850, 11937, 12050, 12094, 12414, 17104, 17168,
    17192, 18454, 18646,
]

themes = {
    1: 'presence', 2: 'faith_doubt', 3: 'mystical', 4: 'mystical',
    5: 'mystical', 6: 'illness_healing', 7: 'illness_healing',
    8: 'scrupulosity', 9: 'self_blame', 10: 'depression',
    11: 'despair', 12: 'depersonalization', 13: 'anxiety',
    14: 'anxiety', 15: 'inner_conflict', 16: 'inner_conflict',
    17: 'melancholy', 18: 'conversion', 19: 'spiritual_experience',
    20: 'spiritual_experience', 21: 'conversion', 22: 'conversion',
    23: 'conversion', 24: 'conversion', 25: 'conversion',
    26: 'conversion', 27: 'relapse', 28: 'conversion',
    29: 'depression', 30: 'despair', 31: 'revival', 32: 'prayer',
    33: 'mystical', 34: 'asceticism', 35: 'asceticism',
    36: 'mystical', 37: 'mystical', 38: 'mystical', 39: 'mystical',
    40: 'spiritual_practice', 41: 'spiritual_practice', 42: 'mystical',
    43: 'faith_doubt', 44: 'suffering', 45: 'transformation',
    46: 'presence', 47: 'spiritual_experience',
}

def find_block(target, cands):
    return min(cands, key=lambda b: abs(b['start_line'] - target))

source_ref = "James, W. (1902). The Varieties of Religious Experience. Longmans, Green & Co. (Public domain.)"

dataset = []
seen = set()
for sl in selected_lines:
    b = find_block(sl, candidates)
    key = b['text'][:100]
    if key not in seen:
        seen.add(key)
        dataset.append(b)

with open(sys.argv[2], 'w', newline='', encoding='utf-8') as f:
    w = csv.DictWriter(f, fieldnames=['id', 'text', 'theme', 'word_count', 'source'])
    w.writeheader()
    for i, b in enumerate(dataset, 1):
        w.writerow({'id': f'james_{i:02d}', 'text': b['text'],
                    'theme': themes.get(i, 'other'), 'word_count': b['word_count'],
                    'source': source_ref})
print(f"Wrote {len(dataset)} rows.")
)"

py_file <- tempfile(fileext = ".py")
writeLines(py_script, py_file)
out <- file.path("inst", "extdata", "varieties_testimonials.csv")
system2("python3", c(py_file, dest, out))
cat("Written to", out, "\n")

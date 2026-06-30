# Data Security Protocol: LLM-Assisted Text Coding

**HuppertLab, Hebrew University of Jerusalem**
*For use with `llmtextcoder`. Adaptable for other labs using the OpenAI API.*

---

## Part 1 — Ethics Summary (for IRB submission)

### What data is processed and where it goes

Participant free-text responses are sent to the OpenAI API for automated
coding against a researcher-defined rubric. The text is the only data
transmitted. No identifiers, demographics, or linkage files are sent. Each
response is submitted as an isolated string with no metadata; the model has
no way to connect it to a participant or to any other response in the study.

### Why this is safe

The workflow provides **three independent layers of protection**, so that a
failure at any one layer does not expose identifying information.

**Layer 1 — Participants are asked not to disclose identifying information.**
Study instructions explicitly request that participants avoid including names,
contact details, or other identifying information in their responses. Our
own review of approximately 300 responses found no clear identifying
information, indicating that participants follow this instruction well in
practice.

**Layer 2 — Our software screens every response before it leaves the lab.**
Before any text is sent to the API, the `llmtextcoder` package runs an
automated PII scan. The scan flags emails, phone numbers, national IDs, and
name/location disclosure phrases (e.g., "my name is …", "I live in …").
Flagged responses are held for researcher review and, where necessary,
redacted in code before any data is transmitted. This check is a required
step in the standard workflow — skipping it requires a deliberate change to
the analysis script and produces a visible code record of that decision.
Crucially, all API calls go through `llmtextcoder`: researchers do not
interact with the API directly, so this screening step cannot be
accidentally bypassed.

**Layer 3 — Transmission is governed by a university-level data agreement.**
All API calls run through the university's ChatGPT Edu account. Under the
ChatGPT Edu agreement:

- OpenAI is **contractually prohibited from using submitted content to train
  or improve its models.** Responses influence nothing beyond the immediate
  scoring task.
- Access is controlled by the university, not individual researchers. The
  university can revoke access immediately and receives usage reports.
  A researcher cannot inadvertently route data through a personal account
  because the laboratory software is configured with institution-issued
  credentials.
- We use the API endpoint, not the ChatGPT chat interface. API submissions
  are not stored in a browsable chat history and cannot be read by other
  members of the organization. Each call is processed in isolation and the
  response is returned directly to the researcher's computer.

This is a fundamental difference from consumer services such as the free
ChatGPT website, where content may be used for training, is stored in a
personal chat history, and there is no institutional oversight.

OpenAI's adherence to these commitments is verified by independent
third-party audits and internationally recognised certifications:

| Standard | Scope |
|----------|-------|
| **SOC 2 Type II** | Independent audit confirming controls for security, availability, confidentiality, and privacy |
| **ISO/IEC 27001:2022** | Information Security Management Systems (ISMS) |
| **ISO/IEC 27017:2015** | Security controls for cloud services |
| **ISO/IEC 27018:2019** | Protection of personally identifiable information in public cloud environments |
| **ISO/IEC 27701:2019** | Privacy Information Management System, extending ISO 27001 with privacy governance |
| **ISO/IEC 42001:2023** | Artificial Intelligence Management System for responsible AI governance |

Additional technical protections include AES-256 encryption at rest and
TLS 1.2+ encryption in transit, configurable data retention policies, and
contractual support for GDPR and FERPA compliance.

**The scored outputs are stored on university computers.**
Results are written directly to researcher-controlled storage. Scores are
never persisted on OpenAI's infrastructure.

### Defense-in-depth: why multiple layers matter

No single control is assumed to be perfect. Participants occasionally include
identifying information despite instructions; automated screening may miss an
unusual pattern. Layers 1 and 2 work to prevent any such information from
entering the transmission in the first place. Layer 3 is the backstop: even
if a fragment of identifying information slips through, the ChatGPT Edu
agreement ensures it cannot be used for training, is not retained in any
accessible form, and cannot be read by anyone outside the immediate scoring
task. No single point of failure can lead to a meaningful privacy breach.

### Summary of safeguards

| Concern | Protection |
|---------|------------|
| Participant data used to train AI | Prohibited by ChatGPT Edu contract |
| Participant discloses their identity | Participant instructions (Layer 1) + automated screening with mandatory researcher review (Layer 2) |
| Researcher uses an unsanctioned account | Workflow enforces institution-issued credentials (Layer 3) |
| Results accessible to third parties | Stored on university systems only |

---

## Part 2 — Student Protocol

Students must satisfy two non-negotiable requirements before any participant
data is processed. The package vignette
(`vignette("running-a-study", package = "llmtextcoder")`) explains the
technical steps in detail; this section specifies what is required and why.

### Requirement 1 — Use `llmtextcoder` for all API calls

Never send participant data to the API directly, through ChatGPT.com, the
ChatGPT app, or any personal AI account. All calls must go through
`llmtextcoder`, which enforces the correct account configuration and ensures
the PII screening step cannot be skipped.

Your API key must come from the **Security Manager**. Do not create a key
from a personal OpenAI account.

### Requirement 2 — Screen and redact PII before scoring

Before any data leaves your computer, you must:

1. Run `scan_pii(df)` and read every finding carefully.
2. Run `redact_pii(df)` to auto-redact structural identifiers (emails,
   phones, IDs).
3. For each flagged name or location phrase, use `redact_words()` to remove
   the identifying words — the number of words to remove is a judgment call
   per finding.
4. Re-run `scan_pii(df)` to confirm no findings remain.

All redaction must live in your analysis script as a permanent record of
every decision. Never edit the source data file directly.

See the vignette (§7) for worked examples of each step.

### Never

- Use ChatGPT.com, the ChatGPT app, or any personal AI account for
  participant data.
- Skip or bypass the PII screening steps.
- Share, email, or commit your API key.
- Edit source data files — all changes must be in code.

### If something goes wrong

Contact the **Laboratory Security Manager** and the **Principal Investigator**
immediately. Do not attempt to resolve the issue independently.

---

## Part 3 — Security Manager Responsibilities

The Security Manager is responsible for:

- Issuing API keys to approved researchers and revoking them on project
  completion or personnel departure.
- Ensuring all active keys belong to the university's ChatGPT Edu
  organization — no personal keys.
- Maintaining the approved `llmtextcoder` version and ensuring all
  researchers use it for every API call; rubric templates are developed
  by researchers and do not require central approval.
- Reviewing OpenAI's data processing terms annually and after any policy
  change; notifying the PI if terms affecting participant data protection
  have changed.
- Responding to any reported incidents and notifying the PI.

API keys must be individually assigned (not shared), rotated at least
annually, and revoked immediately when no longer needed.

# Working on Approve

Approve is an Emacs package for reviewing GitHub pull requests. Read
[docs/index.org](docs/index.org) first: it maps the docs and says what to read
for each kind of task.

## Before you change anything

- Goals and principles: [docs/project.org](docs/project.org). Every change is
  measured against them: functional, flexible, fast, beautiful, stable.
- Decisions: [docs/decisions.org](docs/decisions.org). Build on accepted ones.
  Don't contradict one silently: propose a new entry and ask.
- The next task: the first unchecked item of the current phase in
  [docs/roadmap.org](docs/roadmap.org). Read the flows (`docs/flows.org`) and
  mockups (`docs/ui.org`) it links.
- How to write, build and test: [docs/development.org](docs/development.org).

## Rules that are easy to miss

- The truth is in the code. Verify it with tools; don't rely on an earlier
  conversation. Ask when the docs don't answer a design question.
- Layers only call downwards (docs/architecture.org, "Layers"). Renderers
  never call the network; nothing but `approve-api.el` calls ghub.
- Durable state lives on GitHub, including pending reviews (D02). Never
  require a local clone: everything works through the API, and a clone only
  adds extras, in their own modules (D36).
- Faces only in `approve-faces.el`, inheriting from magit or built-in faces;
  icons only through `approve-icon`.
- Files are split by concern, never by length (D47): what belongs together
  stays together, however long the file. Put new code in the module of
  its concern; don't propose splitting a file because it is long.
- Evil is optional: never `require` it; bindings go in `approve-evil.el`.
- Tests: buttercup, no network, fixtures in `tests/fixtures/`.
  `make build`, `make test` and `make lint` must pass before a commit.
  After changing a mutation, also run the live check on the scratch
  repository, `dev/scratch.el` (docs/development.org, "Scratch repository").
- Conventional commits, one logical change each, with its tests and doc
  updates. Tick the roadmap task in the same commit.
- After each unit of work, reload Approve in the author's running Emacs so
  it can be tried at once: `make build`, then `(load-file
  "~/workspace/approve/dev/load.el")` through the Emacs tools, and check
  `*Messages*`. Reloading loses nothing important; don't ask first.
- A changed mockup or flow is changed in its org file and in its HTML view
  (`docs/html/`); `make docs` rebuilds the flow diagrams (graphviz).
- The shell is fish: wrap loops in `bash -c '…'`. Fish has no heredocs:
  write text to a file first (`git commit -F /tmp/msg.txt`).
- Every command runs under `timeout`, so nothing can hang the session:
  `timeout 600 make test > /tmp/test.log 2>&1`, `timeout 60 emacs --batch …`,
  `timeout 60 gh api …`. Give the tool call a timeout too, and read long
  output from a file. See docs/development.org, "Working as an agent".

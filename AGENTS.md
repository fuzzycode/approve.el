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
- Durable state lives on GitHub, including pending reviews (D02). API-only:
  never depend on a local clone (D03).
- Faces only in `approve-faces.el`, inheriting from magit or built-in faces;
  icons only through `approve-icon`.
- Evil is optional: never `require` it; bindings go in `approve-evil.el`.
- Tests: buttercup, no network, fixtures in `tests/fixtures/`.
  `make build`, `make test` and `make lint` must pass before a commit.
- Conventional commits, one logical change each, with its tests and doc
  updates. Tick the roadmap task in the same commit.
- The shell is fish: wrap loops and heredocs in `bash -c '…'`.

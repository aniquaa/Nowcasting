# AdaptNowcast

> [!WARNING]
> **🚧 Beta — Active Development 🚧**
>
> **AdaptNowcast is in-development and not production-ready.** The API, methods, and outputs are unstable and subject to breaking changes without notice. Nowcasts produced by this software should **not** be used for operational decision-making or public health action at this stage. Validation is ongoing. Use at your own risk.

---

Adaptive method selection for real-time epidemiological nowcasting.

AdaptNowcast continuously monitors reporting delays and automatically selects the appropriate delay-adjustment approach given the most up-to-date data. Rather than committing to a single fixed estimator, it conditions the choice of method on current data conditions — delay stability, reporting completeness, and recent revision behavior — so the adjustment adapts as the reporting situation changes week to week.

## Status

This repository accompanies in-progress methodological and infrastructure work. Expect incomplete features, placeholder interfaces, and changing internals.

- [ ] Core delay-adjustment method wrappers
- [ ] Diagnostic feature extraction from the reporting triangle
- [ ] Method-selection logic
- [ ] Retrospective validation harness (backfill-based)
- [ ] Documentation and worked examples
- [ ] Stable public API

## Design principles

A few commitments that shape the architecture:

- **Deterministic estimation.** The nowcasts themselves are produced by validated, reproducible methods. The adaptive layer chooses *which* method to apply; it does not produce the numbers itself.
- **Reproducibility.** Given identical inputs, the system returns an identical method choice and an identical nowcast. The selection rule is deterministic and auditable.
- **Explainability.** Each selection is accompanied by a rationale describing the data conditions that drove it, so a method choice can be inspected and defended after the fact.
- **Validated as an estimator.** Method selection is treated as a model component and evaluated retrospectively against backfilled data — benchmarked against both the best single method and a static ensemble — not assumed to help.

## Installation

> Not yet available. Installation instructions will be added once the package stabilizes.

## Caveats

Real-time nowcasts are estimates of a present that has not yet fully reported. "Appropriate" method selection is made on observable proxies (data diagnostics and situational context), not on truth, which is unavailable at decision time. All outputs carry uncertainty and are subject to revision as data backfills.

## License

> To be determined.

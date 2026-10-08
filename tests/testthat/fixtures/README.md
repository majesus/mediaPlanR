# Published numeric benchmarks

The two CSV files described below are local-only files of the maintainer: they
are not tracked by git, not published in the repository and not distributed in
the package archive. The tests that read them are skipped when they are absent.
This README records how they were built, so that anyone with access to the
sources can rebuild them.

`cbd_published_plans.csv` contains all 40 two-vehicle schedules from Kim (2005),
Appendix B, printed pp. 184-195, and all 40 two-vehicle schedules from Hong
(1998), Appendix E, printed pp. 622-632. There are two insertions per vehicle.
The earlier fixture omitted Hong schedules 15, 24, 26, 29, 30, 33, 34 and 37;
these have now been transcribed by visual review of the scanned source pages.
No discrepancy-based exclusion is applied to these 80 schedules.

`kim2005_appB_models.csv` contains the same 40 Kim schedules with published
CANX, CSD and MSAD percentages. Input reaches are proportions; output columns
are percentages. `dup` is a reconstructed input, not a directly published
observation. Published outputs are retained without renormalizing or replacing
negative cells: Kim schedule 19 has a CANX four-exposure value of -0.01 percent.

For Kim, duplications were fitted to the rounded CANX column. An independent
reconstruction uses the exact two-insertion marginal probabilities
`c(1-R2, 2*(R2-R1), 2*R1-R2)`, their means and variances, and the canonical
joint probabilities `f(i)*g(j)*(1+rho*z(i)*z(j))`. The five collapsed cells are
linear in `rho`; least-squares fitting and zero-cell fitting need not give the
same duplication after source rounding. Finally,
`dup = R1_1*R1_2 + rho*sqrt(R1_1*(1-R1_1)*R1_2*(1-R1_2))`.

For Hong, the fitted target is the published MSAD reach `R = 1 - MSAD0/100`.
Writing `p=R1_1`, `q=R1_2`, `r=R2_1`, `s=R2_2`, inversion of the two-vehicle
Morgensztern formula gives:

```
Q = ((r+s)^2/R - (r+s))*p*q/(r*s)
dup = Q*(p+q)/(p+q+Q)
```

The additional Hong rows use the printed MSAD zero cells 93.94, 58.32, 75.67,
91.79, 93.75, 93.02, 92.04 and 94.00 percent, respectively. The original 32
rows retain their previously rounded fitted duplications.

CANEX comparisons on Kim are consistency checks of the reconstruction; CBD,
CSD and MSAD compare against other published columns. None constitutes an
independent validation against raw audience measurements. The unresolved
differences and redistribution record are in `inst/DATA-PROVENANCE.md`.

Kim plan 19's MSAD row is tested separately: random duplication, 0.000615,
reproduces its five percentages after rounding to two decimals. The common
CANX-reconstructed duplication, 0.0000623, is retained in this fixture; changing
it for all models would obscure the difference between the printed columns.
The source's reason for the different MSAD input remains unverified.

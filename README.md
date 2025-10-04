# MSc_Predictive_Analytics_HS25_Group_Assignment
Propensity &amp; uplift modeling to grow caravan insurance via targeted cross-sell. Clean data, engineer features, handle imbalance, train/calibrate (LogReg, GBM, CatBoost), evaluate AUC/PR, Lift@K, Profit@Budget. Deliver ranked target list, reason codes, profit curves, and a reproducible scoring pipeline (R).

Caravan Insurance Uplift & Propensity Modeling

Predictive modeling project for a well-established insurer to grow market share and profitability in the caravan insurance segment by targeting the most likely (and most profitable) customers from the existing base.

🎯 Objectives

Primary: Rank current customers by likelihood to purchase caravan insurance (propensity) and maximize expected incremental profit under marketing budget constraints.

Secondary: Produce actionable target lists and explainable drivers to support campaign design and compliance.

KPIs: AUC/ROC, PR-AUC, Precision@K, Lift@K, Expected Profit@Budget, Calibration (Brier), Uplift (Qini/AUUC) for treatment design if test/control is available.

🧠 Business Framing

Segment focus: Caravan insurance currently a small share; large cross-sell potential to existing customers.

Use case: Targeted campaign (email/SMS/agents) to customers without caravan cover.

Constraints: fixed/rolling marketing budget, contact limits, and fairness/compliance guidelines.

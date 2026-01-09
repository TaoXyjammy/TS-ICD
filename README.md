# TS-ICD: Time-Series Iterative Causal Discovery

TS-ICD is a constraint-based causal discovery framework designed for **high-dimensional multivariate time-series data with temporal dependencies and latent confounding**.  
The method extends Iterative Causal Discovery (ICD) by explicitly incorporating **temporal prior constraints**, enabling efficient recovery of **Partial Ancestral Graphs (PAGs)** that respect time-ordering and physiological plausibility.

This repository provides a **fully reproducible implementation** of TS-ICD, together with simulation studies, real-world wearable data experiments, and causal mediation analyses.

---

## ✨ Key Features

- **Time-aware causal discovery** under latent confounding
- Learns **PAGs** with both instantaneous and time-lagged causal relations
- Subject-wise **feature enhancement transformation** for panel time series
- Strong reduction in **conditional independence (CI) tests**
- Supports **anytime inference** via incremental ICD-style search
- Physiologically interpretable results validated by **mediation analysis**

---

## 📐 Method Overview

Given multivariate time-series data from multiple subjects, TS-ICD proceeds in three steps:

1. **Feature Enhancement Transformation**  
   A subject-wise mapping  
   \[
   \mathcal{T}: D \rightarrow \mathcal{Z}
   \]
   embeds raw panel time series into a lag-augmented feature space that satisfies the assumptions of constraint-based causal discovery.

2. **Temporal Prior Masking**  
   The irreversibility of physical time is encoded as structural constraints that forbid backward-in-time causal edges, drastically pruning the search space.

3. **Iterative Causal Discovery (ICD)**  
   An incremental constraint-based algorithm recovers a **PAG** that captures:
   - instantaneous causal effects
   - time-lagged causal propagation
   - latent confounding

---

## 📁 Repository Structure

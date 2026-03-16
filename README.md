# 🍊 Orange Hoops — Syracuse Basketball Data Science Challenge

**Authors:** Caden Lippie & Ethan Radecki

A data-driven late-game play recommendation tool built for Syracuse Basketball. Given a game situation — needing 2 or 3 points — the app identifies the **best player to get the ball to** and **recommends a play design** tailored to that player's archetype.

Live Shiny App: [cadenlippie.shinyapps.io/SUBBALL_Data_Comp_2024](https://cadenlippie.shinyapps.io/SUBBALL_Data_Comp_2024/)

---

## 📸 App Screenshots

**Need 3 Points — Syracuse vs. opponent (Chris Bell recommended)**

<img width="950" height="400" alt="3pt Scenario" src="https://github.com/user-attachments/assets/f0473340-b6b3-46f7-9d59-4061cf2454e0" />


**Need 2 Points — Syracuse vs. opponent (Judah Mintz recommended)**

<img width="950" height="400" alt="2pt Scenario" src="https://github.com/user-attachments/assets/baf5347b-c4da-489e-b718-3c835399e4a7" />


---

## 🏀 Project Overview

### Problem
In late-game situations, coaches need to quickly decide: *who gets the ball, and what play do we run?* This tool uses 2023-24 college basketball play-by-play data to answer that question for Syracuse and its opponents.

### Assumptions
- 2023-24 rosters are treated as equivalent to 2024-25 rosters (due to extensive transfer portal movement and limited data on incoming freshmen)
- Free throws are excluded — the focus is on field goal scoring only
- Data covers the 2023-24 CBB regular season and conference tournaments (no NIT or March Madness)

---

## 📊 Methodology

### Step 1 — Data Collection
Play-by-play data (`PBP2324.csv`) is filtered to games involving Syracuse and its 2024-25 opponents. Advanced stats (usage rate, height, position) are scraped from [Sports Reference](https://www.sports-reference.com/cbb/).

### Step 2 — Feature Engineering

The following weighted metrics are computed for each player:

| Metric | Definition |
|---|---|
| **Weighted Two Pointers** | 2P% × √(2PA) |
| **Weighted Three Pointers** | 3P% × √(3PA) |
| **Weighted End-of-Shot-Clock Scoring** | FG% on shots with ≤2 seconds on shot clock × √(attempts) |
| **Weighted Halftime Closers** | FG% on shots with ≤5 seconds in 1st half × √(attempts) |
| **Weighted Game Winners** | FG% on shots with ≤5 secs and score within 5 × √(attempts) |
| **Weighted Clutch Scoring** | FG% with ≤2 minutes left and score within 5 × √(points scored) |

> Weighting by the square root of attempts balances efficiency with sample size.

Shot clock time is reconstructed from play-by-play timestamps, with adjustments for offensive rebounds (20-second reset) and blocked shots.

### Step 3 — Player Recommendation (Pareto Front Optimization)

A **Multi-Objective Optimization (Pareto Front)** approach is used to identify the best player for each scenario. Players on the Pareto front are then scored using a weighted composite:

**Need 2 Points:**
| Factor | Weight |
|---|---|
| Weighted 2-Pointers | 40% |
| Usage Rate | 20% |
| Weighted 3-Pointers | 10% |
| Game Winners | 10% |
| Clutch Scoring | 10% |
| Shot Clock Scoring | 5% |
| Halftime Closers | 5% |

**Need 3 Points:** Same factors, but Weighted 3-Pointers at 40% and Weighted 2-Pointers at 10%.

### Step 4 — Player Clustering (K-Means)

Players are clustered into **4 archetypes** using K-Means (k=4, selected via elbow method) on:
- Usage Rate
- Height
- Shot frequency breakdown (% threes, % 2pt jumpers, % layups, % dunks)
- Assisted shot percentage

**Cluster Archetypes:**

| Cluster | Avg. Height | Shot Profile |
|---|---|---|
| **Ball Handling Combo Guard** | 6'3" | 36% 3s, 35% 2pt jumpers, 27% layups — 23% USG |
| **Shooter** | 6'5" | 60% 3s, 18% 2pt jumpers, 20% layups — 17% USG |
| **Three Level Scoring Forward** | 6'8" | 21% 3s, 23% 2pt jumpers, 49% layups — 19% USG |
| **Roll and Cut Big** | 6'10" | 9% 3s, 26% 2pt jumpers, 45% layups, 20% dunks — 18% USG |

PCA is used for 2D visualization of cluster separation.

### Step 5 — Play Design Matching

Each player archetype is matched to a play design best suited to create their optimal shot:

| Archetype | Play | Goal |
|---|---|---|
| Ball Handling Combo Guard | 5-Out Isolation | Mid-range jumper or layup off the drive |
| Shooter | Flare Screen | Corner three or mid-range jumper |
| Three Level Scoring Forward | UCLA Cut | Driving opportunity or three |
| Roll and Cut Big | Spain Pick and Roll | High-percentage look near the basket |

---

## 📦 Dependencies

```r
library(rvest)       # Web scraping
library(rPref)       # Pareto front optimization
library(tidyverse)   # Data manipulation
library(factoextra)  # Clustering visualization
library(shiny)       # App framework
library(shinydashboard)
library(DT)
```

---

## 🖥️ Shiny App

The interactive Shiny app allows users to:
1. Select whether they **need 2 or 3 points**
2. Select a **team**
3. See the **recommended player** (with archetype label)
4. See the **recommended play diagram** with a description of the player's role

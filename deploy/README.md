# Deployment Guide

MicrobiomeExplorer can be deployed as a web app so anyone can use it in a
browser without installing R.

## Option 1: Hugging Face Spaces (Recommended)

Free, no credit card, 16 GB RAM, no usage caps. Best for public/sustained
access. The only downside is a 2-5 minute cold start after 48 hours of
inactivity.

### Quick Start

1. Create a free account at [huggingface.co](https://huggingface.co)
2. Create a new Space at [huggingface.co/new-space](https://huggingface.co/new-space):
   - **Space name:** MicrobiomeExplorer
   - **SDK:** Docker
   - **Visibility:** Public
3. Run the deploy script:

```bash
./deploy/deploy_huggingface.sh YOUR_HF_USERNAME
```

The first build takes ~30-40 minutes while Bioconductor packages compile. After
that, the app is live at:

```
https://huggingface.co/spaces/YOUR_HF_USERNAME/MicrobiomeExplorer
```

### Manual Deployment

If you prefer not to use the script:

1. Clone your HF Space repo:
   ```bash
   git clone https://huggingface.co/spaces/YOUR_USERNAME/MicrobiomeExplorer
   ```
2. Copy the project files into the cloned repo
3. Copy `deploy/Dockerfile.huggingface` as `Dockerfile` in the Space repo root
4. Copy `deploy/README_huggingface.md` as `README.md` in the Space repo root
5. Commit and push

### Files

| File | Purpose |
|------|---------|
| `Dockerfile.huggingface` | Docker config targeting port 7860 (HF requirement) |
| `README_huggingface.md` | Space metadata (title, SDK, license) |
| `deploy_huggingface.sh` | Automated deploy script |

---

## Option 2: shinyapps.io (Backup)

Free tier gives 25 active hours/month and 1 GB RAM. Good for quick demos but
limited for sustained public use.

### Setup

1. Create a free account at [shinyapps.io](https://www.shinyapps.io)
2. Get your token from the dashboard
3. Configure credentials in R:

```r
rsconnect::setAccountInfo(
  name   = "YOUR_ACCOUNT",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)
```

4. Install the package locally, then deploy:

```r
# Make sure MicrobiomeExplorer is installed first
source("deploy/deploy_shinyapps.R")
```

Alternatively, deploy the standalone wrapper (`deploy/app.R`) which
auto-installs the package from GitHub on the shinyapps.io server.

### Files

| File | Purpose |
|------|---------|
| `app.R` | Standalone wrapper that installs from GitHub |
| `deploy_shinyapps.R` | Deploy script using locally installed package |

---

## Option 3: Docker (Self-Hosted)

Use the project root `Dockerfile` to run anywhere Docker is available:

```bash
docker build -t microbiome-explorer .
docker run -p 3838:3838 microbiome-explorer
```

Then open `http://localhost:3838` in your browser.

---

## Platform Comparison

| Platform | RAM | Usage Cap | Cold Start | Cost |
|----------|-----|-----------|------------|------|
| **HF Spaces** | 16 GB | None | 2-5 min after 48h idle | Free |
| shinyapps.io | 1 GB | 25 hrs/month | 30-60s | Free |
| Docker (self) | Your machine | None | None | Your hardware |

# CR8digger

A modern DJ track discovery platform built with Phoenix LiveView. Find new tracks through intelligent audio feature matching, natural language descriptions, and earn affiliate revenue when users purchase recommended tracks.

## Features

- **Smart Search**: Search by track/artist name with Spotify integration
- **Natural Language Discovery**: Describe the vibe you want ("dark minimal techno around 125 BPM")
- **Audio Feature Analysis**: See BPM, key, energy, danceability, mood, and more
- **Intelligent Recommendations**: Fine-tune recommendations with sliders for energy, danceability, tempo, etc.
- **Crate Building**: Save tracks to your personal crate
- **Affiliate Links**: Earn commission when users buy tracks through Beatport, Juno, Apple Music, etc.
- **Click Tracking**: Track affiliate conversions and revenue

## Tech Stack

- **Backend**: Elixir/Phoenix 1.8
- **Frontend**: Phoenix LiveView + Tailwind CSS + DaisyUI
- **Database**: PostgreSQL
- **APIs**: Spotify, Claude/OpenAI (LLM), Beatport, Juno, Apple Music, Amazon

---

## API Setup Guide

### Required APIs

#### 1. Spotify API (Core - Required)

**Cost**: Free (with rate limits)

**What you get**:
- Track search
- Audio features (BPM, key, energy, danceability, valence, etc.)
- Recommendations engine
- 30-second audio previews

**Signup Steps**:
1. Go to [Spotify Developer Dashboard](https://developer.spotify.com/dashboard)
2. Log in with your Spotify account (create one if needed)
3. Click "Create App"
4. Fill in:
   - App name: "CR8digger" (or your name)
   - App description: "DJ track discovery tool"
   - Redirect URI: `http://localhost:4000/auth/spotify/callback`
   - Select "Web API" checkbox
5. Accept terms and create
6. Go to Settings → View client ID and secret

**Environment Variables**:
```bash
SPOTIFY_CLIENT_ID=your_client_id
SPOTIFY_CLIENT_SECRET=your_client_secret
```

**Rate Limits**:
- Development mode: ~100 requests/hour per user
- Extended quota: Apply via dashboard for production

**Important Note** (as of 2025): Spotify now primarily accepts applications from organizations, not individuals. For personal/development use, the default quota should be sufficient.

---

### Optional APIs (Choose Based on Needs)

#### 2. LLM API (Natural Language Discovery)

Choose ONE of these:

##### Option A: Anthropic Claude (Recommended)

**Cost**: Pay-as-you-go (~$3-15/million tokens depending on model)

**Signup Steps**:
1. Go to [Anthropic Console](https://console.anthropic.com)
2. Create account and verify email
3. Add payment method
4. Go to API Keys → Create Key

**Environment Variable**:
```bash
ANTHROPIC_API_KEY=sk-ant-...
```

##### Option B: OpenAI GPT

**Cost**: Pay-as-you-go (~$0.15-60/million tokens depending on model)

**Signup Steps**:
1. Go to [OpenAI Platform](https://platform.openai.com)
2. Create account
3. Add payment method in Billing
4. Go to API Keys → Create new secret key

**Environment Variable**:
```bash
OPENAI_API_KEY=sk-...
```

---

### Affiliate Programs (Revenue Generation)

These are optional but enable you to earn money when users purchase tracks.

#### 3. Beatport Affiliate Program

**Commission**: 5-10% on sales

**Signup Steps**:
1. Go to [Beatport Partner Portal](https://partnerportal.beatport.com)
2. Click "Sign Up"
3. Fill in your details and website info
4. Wait for approval (usually 1-3 business days)
5. Once approved, get your affiliate ID from the dashboard

**Environment Variable**:
```bash
BEATPORT_AFFILIATE_ID=your_affiliate_id
```

---

#### 4. Juno Download Affiliate Program

**Commission**: 5-10% + £5 signup bonus + £5 for first 50 clicks

**Cookie Duration**: 14-28 days

**Signup Steps**:
1. Go to [Juno Affiliate Program](https://affiliate.juno.co.uk/affiliates/)
2. Click "Sign Up"
3. Fill in website and payment details
4. Get approved (usually quick)
5. Get your affiliate ID/ref code

**Environment Variable**:
```bash
JUNO_AFFILIATE_ID=your_affiliate_id
```

---

#### 5. Apple Music Affiliate Program

**Commission**: 7% on qualifying purchases

**Cookie Duration**: 30 days

**Signup Steps**:
1. Go to [Apple Services Performance Partners](https://performance-partners.apple.com)
2. Apply for the program
3. You'll need:
   - Website URL
   - Traffic estimates
   - Content description
4. Wait for approval (can take 1-2 weeks)
5. Once approved, get your affiliate token

**Environment Variable**:
```bash
APPLE_AFFILIATE_TOKEN=your_token
```

---

#### 6. Amazon Associates

**Commission**: 5% on digital music + bounties for Prime/Unlimited signups

**Signup Steps**:
1. Go to [Amazon Associates](https://affiliate-program.amazon.com)
2. Sign up with your Amazon account
3. Fill in website info
4. Get your Associate Tag immediately
5. Note: Must make 3 sales within 180 days to stay active

**Environment Variable**:
```bash
AMAZON_ASSOCIATE_TAG=your-tag-20
```

---

#### 7. Traxsource Affiliate (via Yeesshh)

**Commission**: Varies

**Signup Steps**:
1. Contact Traxsource or sign up via [Yeesshh network](https://yeesshh.com)
2. This is more selective - may need established traffic

**Environment Variable**:
```bash
TRAXSOURCE_AFFILIATE_ID=your_id
```

---

## Cost Summary

| Service | Cost | Notes |
|---------|------|-------|
| Spotify API | Free | Rate limited, may need extended quota |
| Anthropic Claude | ~$0.003/request | Pay as you go |
| OpenAI GPT | ~$0.001-0.06/request | Pay as you go |
| Beatport Affiliate | Free | Earn 5-10% commission |
| Juno Affiliate | Free | Earn 5-10% commission |
| Apple Affiliate | Free | Earn 7% commission |
| Amazon Associates | Free | Earn 5% commission |

**Minimum to get started**: Just Spotify (free)

**Recommended setup**: Spotify + Claude/OpenAI + Beatport + Juno

---

## Local Development Setup

### Prerequisites

- Elixir 1.15+
- Erlang/OTP 26+
- PostgreSQL 14+
- Node.js 18+ (for assets)

### Installation

```bash
# Clone the repo
git clone https://github.com/yourusername/crate-digger.git
cd crate-digger

# Install dependencies
mix deps.get

# Create and migrate database
mix ecto.setup

# Install Node.js dependencies (for asset building)
cd assets && npm install && cd ..

# Start the server
mix phx.server
```

### Environment Variables

Create a `.env` file or set these in your shell:

```bash
# Required
export SPOTIFY_CLIENT_ID=your_spotify_client_id
export SPOTIFY_CLIENT_SECRET=your_spotify_client_secret

# Optional - LLM (choose one)
export ANTHROPIC_API_KEY=your_anthropic_key
# OR
export OPENAI_API_KEY=your_openai_key

# Optional - Affiliates
export BEATPORT_AFFILIATE_ID=your_beatport_id
export JUNO_AFFILIATE_ID=your_juno_id
export APPLE_AFFILIATE_TOKEN=your_apple_token
export AMAZON_ASSOCIATE_TAG=your-tag-20
export TRAXSOURCE_AFFILIATE_ID=your_traxsource_id
```

Visit [http://localhost:4000](http://localhost:4000) to start discovering tracks!

---

## Database Schema

```
tracks
├── spotify_id (unique)
├── title, artist, album
├── image_url, preview_url, spotify_uri
├── bpm, key, mode
├── energy, danceability, valence
├── acousticness, instrumentalness, speechiness, liveness
├── loudness, time_signature, popularity

users
├── email, username (unique)
├── hashed_password

crates
├── name, description
├── is_public
├── user_id → users

crate_tracks
├── crate_id → crates
├── track_id → tracks
├── position, notes

affiliate_clicks
├── track_id → tracks
├── user_id → users
├── platform, affiliate_link
├── clicked_at, converted
├── commission_amount
├── ip_hash, user_agent

search_history
├── user_id → users
├── query, query_type
├── results_count
├── seed_track_id → tracks
```

---

## Revenue Optimization Tips

1. **Prioritize platforms by commission**: Beatport/Juno (5-10%) > Apple (7%) > Amazon (5%)
2. **Electronic music focus**: Beatport and Traxsource have the best rates for DJ/electronic
3. **Track click-through rates**: Monitor which platforms convert best
4. **Cookie duration matters**: Apple (30 days) > Juno (14-28 days) > Beatport (session)
5. **A/B test button placement**: Try different orders and styles

---

## License

MIT

---

## Credits

Built with Phoenix LiveView, Tailwind CSS, and DaisyUI.

Original concept inspired by the Elm version (2017).

+++
title = "The perversion of AI discourse"
date = 2026-02-22

[taxonomies]
tags = ["ai", "musings"]

+++

I've been thinking lately on what bothers me about how AI is being discussed and why the performative nature of the people in the space bothers me so much (amongst other things).

Over-hype for me has always soured the value of the tech being talked about. It didn't matter much to me when it was about web3/crypto, since I simply didn't care about the tech itself.

For AI though, I feel different. It has quite substantial real-world applications and already brings quite a lot of value, but the overhype is causing the problems and deficiencies of the tech to be overlooked.

We would all benefit from being able to talk honestly and openly about AI, and I think the first step is identifying what is ruining the discourse.

<div></div><!-- more -->

## The perversion

There are several pressures that are causing this perversion, but I mainly see it from two sides.

### There's money on the line

- There's a huge pressure to use this new exciting technology, especially from investors
- So much capital has been poured into AI, that for many companies it simply has to succeed or they will implode
- Companies with money directly on the line are being extremely disingenuous in their claims, and many are just outright lying about the value they are getting out of it

A recent example of being disingenuous is Anthropic's claim of writing a C-compiler in two-weeks.

Most people would probably just have seen the [YouTube video](https://www.youtube.com/watch?v=vNeIQS9GsZ8), which makes impressive claims of building a "fully functional compiler" in two weeks and "walked away", "zero manual coding", and "Tested it on the Linux Kernel - It works".

Now, [the blog post](https://www.anthropic.com/engineering/building-c-compiler) from them is a bit more honest, but even then makes claims such as "This was a clean-room implementation", while also having some honest parts such as "Claude simply cheats here and calls out to GCC for this phase".

The first issue opened in the compiler repo? ["Hello world does not compile"](https://github.com/anthropics/claudes-c-compiler/issues/1).

I don't think I'll do a better job than [ThePrimeagen, on YouTube, explaining](https://www.youtube.com/watch?v=6QryFk4RYaM) why the details in this announcement, while certainly impressive to some extent, is also not reflective of actual Software development at all. Highly recommend the 8:17 minutes on the video.


### Social pressure

- People don't want to seem incompetent so they are boasting and exaggerating claims of the value they get out of it and how they are using it
- It doesn't help much that all the grifters from web3/crypto shifted to shilling in AI
- People that are trying to say "it clearly fails spectacularly here" are seen negatively as not keeping up with progress
- In tech specifically, it massively distorts the view that non-tech people have of what building software actually requires (you now have people saying "SaaS is dead" which couldn't be a stupider take)

So now we are left in the situation where we cannot talk openly about the problems AI has, and I think that is stifling its growth.

The effect of this is that the people that are actually out there building things are not getting their problems solved, and I personally think that massively stifles AI innovation.

To fix these is almost to go against the grain.


## Deficiencies in Software Engineering

For me personally, there are some big deficiencies when it comes to Software Engineering which makes AI quite far from being able to replace anyone competent at engineering yet:

- AI is beyond terrible at detecting patterns and taking a step back to abstract.
  - The solution is almost always more code, which is almost the antithesis of what makes a codebase maintainable.
- It doesn't learn from prior mistakes, reviews, etc.
  - It's like having a junior engineer that has memorized the documentation, but doesn't know how to apply it. You normally grow these engineers over time with code reviews, mentoring, etc. AI doesn't benefit from any of that currently.
- Coding is inherently trying to take something unspecific and create something specific a machine can execute.
  - AI isn't magically reading your mind, just like Engineers never could magically read the mind of a Product Manager. Exploration happens during development, but this fact is lost in the current process we have when using AI.

I think most of these are fixable on the long-term, but not if we aren't honest about them existing.


## Where do I land today?

I use AI daily, it boosts my productivity, just like LSP do, linters do, choosing a good programming language or framework does, etc. It didn't magically solve my problems (like its being sold as), it's a tool that I use to be more productive.

Right now I find that I land somewhere in:

1. Autocomplete and tab-completion are massively helpful
2. In-context handing specific parts over to an Agent can be helpful, if the scope is kept limited. You still understand the code and can rewrite/improve it on the spot before it goes live.
3. Full handoff to an Agent is quite often gonna lead to a subpar result. You now have no idea what is operationally running in production, unless you are extremely thorough at code review (and if I know anything, it's that people are lazy about reading code, myself included)

I live happily somewhere between step 2 and 3, occasionally dabbling in full hand-offs when my brain is tired.

Otherwise I find that I am generally much more precise, fast, and correct in reaching a good solution compared to the AI. Who would have thought that +15 years of Engineering experience still pays off 🤷‍♂️

{{ medium_comments(post="the-perversion-of-ai-discourse-70edeb150178") }}

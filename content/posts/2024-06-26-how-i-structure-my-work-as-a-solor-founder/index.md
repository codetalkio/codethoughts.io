+++
title = "How I structure work as a Solo Founder"
date = 2024-06-26

[taxonomies]
tags = ["project management", "solo founder", "codetalk"]

[extra]
og_image = "how-i-structure-my-work-as-a-solor-founder-initiatives.png"
+++

This is for sure something that will keep evolving over time, but I got inspired by [this Hacker News post](https://news.ycombinator.com/item?id=40742831) to share in more detail how I organize myself and my work (you can see my short answer [here](https://news.ycombinator.com/item?id=40743669)).

The question posed is great, so let’s start from there:

> When working with a team, I've found that Scrum-like short-term goals and organized task backlogs really help keep me focused and on track with the work I have to do. For solo development, however, I've never really found any approach that sticks and often end up getting sidetracked and losing track of my objectives.
>
> So my question to all of you is, what tools and techniques have you used to ensure you stick to your objectives?

You could rephrase this slightly to: How do I run *my own* project management?

For most Engineers, this is not actually something they’ve needed to do often, especially if working in larger corporations. The people that have worked at Startups are a mixed bag, where some learn the value of structure, and others managed to get by without since the fast-paced environment changes things around all the time anyways.

I’ve personally landed in the camp of: **Structure is valuable, even crucial, but it should never get in my way. Best of both worlds sorta.**

<div></div><!-- more -->

{{ toc() }}

## What is my context?

Not every way will fit everyone. That’s actually one of the most important lessons of implementing processes: Make it fit to your reality.

So what is my reality?

I’ve spent many years both as a solo-dev on my hobby projects, and as a leader running multiple teams of Engineers, acting CTO, and even helping managers do the same.

Right now though, I’m a solo founder working on multiple small projects as I figure out both what I want to do, as well as what might realistically allow me to continue doing it forever.

In other words:

- I run many small-to-medium projects
- I have more projects than I have time (like almost all businesses)
- Each project can be a mix of Engineering work, research, a bit of marketing, sales’y activities, etc
- I need to be flexible and able to change focus fast
- I will inherently discover work as I am going through a project
- I want my approach to easily scale up to many people, without being a burden while I’m just myself

## How do I structure my work?

There are many systems you can do this in, and this is entirely up to your preference. I’ve settled on Linear as my go-to Project Management tool, with GitHub for everything code, and Notion for long-term knowledge base. So for this post here, we’ll focus on the Linear bit.

The overall concept is that I make daily/weekly/monthly goals, and structure it in whatever App I use (e.g. Linear in my case)

- **Monthly goals** are very high level and few (e.g. “Reimagine Goals App”, “Redesign and relaunch blog”)
- **Weekly goals** are more tangible and limited (e.g. “Settle on approach for calling Rust from Swift code”, or “Finish design and styling of posts”)
- **Daily goals** are very concrete (e.g. “Set up UniFFI pipeline to generate Swift bindings” or “Implement new theme across blog pages”)

Let’s go into a bit more detail, and make this more concrete to how I use it with Linear so you can get some inspiration and see if this could work for you.

### Monthly goals

The purpose of setting goals at a cadence (be they monthly, quarterly, or yearly) is to make sure you take a step back and think about “Am I on the right track, knowing what I now know?”.

I’m still changing things frequently enough that I’ve settled on a monthly cadence, but at many companies you’ll operate at a quarterly level for most goals, and a yearly level for a few strategic focuses.

At the time of writing, I currently have two focuses for the month of July:

- Launching some of my initial projects for codetalk
- Relaunching my blog with a redesign, since I’m moving it from [codetalk.io](http://codetalk.io) to [codethoughts.io](http://codethoughts.io) now that codetalk is my business (aka corporate) website

In Linear, I’ve structure the monthly goals using their new [Initiatives](https://linear.app/docs/initiatives), which gives me a great way to write a high-level summary of the goal, and gather the related projects that will get me there into each of them.

{{ image(path="how-i-structure-my-work-as-a-solor-founder-initiatives.png", caption="A view of my Initiatives in Linear for July 2024", width=700) }}

A view of my Initiatives in Linear for July 2024.

Initiatives is quite new, so before that I would write down my Monthly goals with pen-and-paper in a Notebook, since the goal setting was meant to be a time of focus and reflection.

I might still initially do that, but they’ll now end up in Linear at the end, giving a nice structure and keeping everything in one place.

While I won’t use Project Updates as long as it’s just myself, I would have loved this at previous companies, working with OKRs. With no better way back then, we resorted to gathering all statuses into Google Slides where every team has their slide, updating a green/yellow/red light, and giving a status update, and setting the date of the last update. This is more or less built directly into Linear’s Initiatives, so that’s nice to see some progress on that front.

### Weekly goals

The weekly goal setting is a bit less systematized than the others, as I use it more as a point of reflection.

My approach so far has been to start every Monday with:

- Opening up the list of issues across all the Initiatives that I currently have active
- Giving a priority (or re-prioritizing) the issues so the are listed in the order that I want to do them in
- Clean up or add any missing issues

I then use this prioritization as the input for my daily goals, and it also makes sure that I continuously maintain the projects and don’t let them get state with irrelevant issues and backlog creep.

In Linear I’ve set up a Custom View with a simple filter on the Initiatives I have active at the moment, named “Initiative Issues” in the screenshot below.

{{ image(path="how-i-structure-my-work-as-a-solor-founder-issues.png", caption="A Custom View with a filter on the currently active Initiatives", width=700) }}

A Custom View with a filter on the currently active Initiatives.

### Daily goals

Arriving at the final step, I start from the “Initiative Issues” list and pick out what I’ll work on today. After selecting 1-3 issues, I mark them with a `Goal` label (or, specifically in my case `M-Goal` where stands for “Meta” which is the grouping it’s under).

These issues then go into another Custom View called “Goal of the day” which just gathers all issues with the `Goal` label on them, as well as having myself assigned to them.

{{ image(path="how-i-structure-my-work-as-a-solor-founder-goals.png", caption="A Custom View with a filter on “Goals", width=700) }}

A Custom View with a filter on “Goals” label being added.

I’ve also set this view as my default view (`Preferences → Default home` view in Linear), which helps keep my focus on the tasks at hand, and not get distracted by all the other things going on when I do my day-to-day work.

**Some questions naturally arise:**

> What happens if you don’t finish a Goal today?

I simply slide it to tomorrow, or evaluate if I should drop it to focus on something else.

> Do you plan multiple days of Goals at the same time?

Sometimes. I might put Goals for tomorrow or the day after into the Backlog status, while keeping the goals of today in a Todo status to differentiate them.

Handling Due Dates in Linear is a bit subpar at the moment, and not as easy/quick as simply updating statuses. I plan to tackle this in one of my upcoming projects hopefully, as a little companion app to Linear, GitHub, and Notion combined.

> Do you plan in further detail?

I often make checklists for what I want to do inside each of the Goals as I’m working on them. E.g. it could be an implementation plan, or simply practical things I need to get through.

That’s a bit more related to my style of working, where I prefer to need as little in my working memory as possible, so that I can drop something without forgetting what I was doing.

For Engineering work, I’d recommend it to everyone, as it makes you think through your plan of attack before just diving in and wasting hours of work on something you could have skipped or will need to rework to fit with a later functionality.

## Is this a bit overkill?

Haha, perhaps! Writing it out in this detail certainly makes it seem like more work than it is!

Once you’ve set up the initial structure, all you really do is:

- Stop once a month to think about what you should be focusing on
- Start the week by thinking about what the immediate focus should be, and tidying things up a bit
- Agree with yourself each day on what you should be focusing on by adding a label to a task

And that’s really all there is to it when you boil it down to the essentials!

The great thing though, is that once I grow beyond just myself, I have all the processes and structure in place to get someone else immediately started helping out with projects and following a similar structure.

## What other systems could I use?

Honestly, most things would work, and you can even just do it with pen-and-paper. I personally prefer things digitally so that I can add or edit something from my phone on-the-go. E.g. I might have an idea randomly while I’m grocery shopping because my mind wanders, and I don’t have my notebook with me there, but I do always have my phone with me.

As an alternative to Linear, I’d probably recommend Notion. You will need to set up the structure more yourself, but you can wrangle their [Projects](https://www.notion.so/product/projects) functionality into something that will allow you a very similar setup.

I always love to get inspiration, so I’d love to hear from you about your own preferred way of organizing yourself.


{{ medium_comments(post="how-i-structure-work-as-a-solo-founder-4944afcfc2f2") }}

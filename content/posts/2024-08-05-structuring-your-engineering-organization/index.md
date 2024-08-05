+++
title = "Structuring your Engineering Organization"
date = 2024-08-05

[taxonomies]
tags = ["organization", "ideas", "engineering", "leadership"]

[extra]
og_image = "og-image.png"
mermaidjs = "true"
+++

From a couple of founders, to a small team, to being a “small startup”, moving into scale-up territory, and finally transitioning beyond, there are many changes your organization will go through.

But what should you aim for as you grow and need to delegate your responsibilities and you transition from a couple of founders, into a company of 300 or more?

Throughout my career I’ve seen many structures at different scales, and have thought a lot about the ideal structure I would land on for a Engineering company that develops Products (e.g. a SaaS company).

I’ll warn you though: This will be opinionated, but I do believe that there is a strong reasoning behind the structure I’ve ended up at—let's dive in!

<div></div><!-- more -->

{{ toc() }}

## Ideal Structure

Without further ado, let’s jump straight into the high-level overview of the structure, before I give a few more details on why we’ve ended up here:

<pre class="mermaid">
graph TD
  %% First Layer
  CEO --> CFO
  CEO --> CCO
  CEO --> CTO[CTO/CTPO]

  %% Second Layer
  CFO --> HR
  CFO --> Legal
  CFO --> Finance

  CCO --> Sales
  CCO --> Marketing

  CTO --> Engineering
  CTO --> Product
  CTO --> CustomerSuccess[Customer Success]

</pre>

There will both be familiar aspects to this structure, as well as a few points that might make you tilt your head—let’s take it from the top!

### CEO (Chief Executive Officer)

The CEO is the place where all decisions ultimately stop, thus all reporting chains should end there.

Each CEO in a company will look different, bringing in unique experience. Some may have run companies at the same scale before, but for many its probably their first time getting to this size.

It’s important that the CEO learns quickly how to delegate the things they are not good at, to people with specialized expertise in the area.

That said, that’s not equivalent to not needing to know about these areas. As the CEO, you should be able to have deep conversations in all the areas, to know when you need to guide your team towards better decisions, and when to give them the reign they need to effectively lead.

It’s neither good to be a decision bottleneck, but it’s also not good to let parts of your company steer off in a different direction than the rest. It’s up to the CEO to find this balance.

There is one thing that I don’t ascribe to, but often hear mentioned to aspiring CEOs from well-intentioned but ill-informed VCs, Boards, etc: “Your reports should not expect guidance and help from you.”

As the CEO, it’s your responsibility to grow your executive teams competences, either directly or via external help. If you don’t take accountability for their performance, then you will ultimately hurt the company. Use that expertise that has put you there in the first place to better your company.

### CFO (Chief Financial Officer)

The core role of the CFO is to be responsible for the Financials of the company. But if we expand the notion of Financials a bit, and take a look at which parts of the company need to deeply interface with this, then we end up with a broader look on things.

**HR:** The Human Resources (sometimes “People”) department consists in large part of Financial and Legal related activities. They are heavily involved in hiring which require legal contracts and budgets, company policies which may impact legal or financial aspects (e.g. providing lunch, or employee confidentiality agreements), company activities such as workshops, events, and more, which all require budget.

By placing them in the same organization as Finances and Legal, we shorten the collaboration loop with the departments that are necessary for HR to function well.

**Legal**: Is small, and greatly benefits from working closely with both Finance and HR. This allows Legal to spar with both of these, as much of the output of the Legal team will need to impact either the people in the company, or requiring deep financial insight into the company to e.g. evaluate risk appetites, etc.

**Finance:** Unsurprisingly, this is naturally under the CFO. We again benefit greatly from having Legal and HR close to Finance.

The CFO should ultimately make all these parts play well together as a team, and its important that they end up working efficiently together, to make each block the other.

### CCO (Chief Commercial Officer)

The CCO is running the commercial side and oversees the whole loop from getting prospects into the funnel, and closing them. Organizations sometimes end up with a CMO, but I believe splitting this position out to be a peer of the Commercial side is a mistake.

Sales and Marketing should be working together, they are essentially both driving towards the same direct goal: Generate more revenue for the company. That’s why they exist.

Sales insights should inform the initiatives Marketing is doing, amongst many other things. What’s important to the customers you’re talking with? Are they hearing the message you’re sending out in the world?

Marketing works closely with sales to make this feedback loop efficiently, and allow them to pivot: Is what we are doing actually working and driving our sales?

That’s not to say that Marketing won’t also be working on other things, and longer bets (such as the brand voice), but ultimately all of this is serving the goal of driving attention to the company and Product, and the frontline that will be impacted by a new successful (or unsuccessful) campaign running, is very likely to be Sales.

### CTO/CTPO (Chief Technology Officer/Chief Technology & Product Officer)

The CTO (or CTPO) is responsible for driving the Product forward and ensuring its quality. This is an end-to-end responsibility, and its important to make the CTO/CTPO feel the pain of delivering a bad product, and also see the upsides of delivering quality work.

**Engineering**: Is under the CTO as expected, and forms the core backbone of the organization. You’ll probably only have Engineering when first starting out before you grow to have dedicated Product Managers/Owners, designers, and more. They will probably also have been first-line support in the very early days.

**Product:** It’s not uncommon to split out Product from Engineering, but I believe this to be the source of a great deal of friction and unnecessary conflict in organizations.

For all intents and purposes, Engineering *is* the Product, until the point where you reach a scale and need to have a Product organization as well. They should also be collaborating extremely tightly, and Engineering should ideally be extremely involved in Product development, avoiding a typical “Ticket closing Engineer” and instead aiming for Engineers that are involved in finding solutions to customer requests and ideas.

**Customer Success:** The last one is a lot more controversial, putting Customer Success/Support under the CTO. Often this ends up being the CCO’s responsibility, but I believe this to be a fundamental mistake and wrong view of how to build a Product.

Much like Product, in your early days your Support will most likely have been your Engineering team. Customer Success/Support is your view into friction and quality issues with your Product, and should ultimately impact your Roadmap, thus creating a feedback loop between Engineering, Product, and Customer Success/Support is highly desirable.

## What are we optimizing for?

If it wasn’t clear from the individual sections, we are strongly optimizing for collaboration and responsibilities in the organization. If two units share the same goals, then they should be under the same leadership to ensure that the accountability for their success ultimately is merged to just one person, as well as having that person ensure that collaboration is as efficient and smooth as possible.

- Who knows better the finer nuances of Marketing and Sales that will make them collaborate well together: The CCO or the CEO?
- Who knows better how the Engineering work and Product development is done, and what is pressuring/causing changes in quality: The CTO/CTPO or the CEO?

It should be clear that I firmly believe it’s the former, specialized role, and not the generalized role of CEO that also holds a much larger set of responsibilities and thus cannot dedicate the time necessary to fix these issues.

Another example: What happens if you have a CCO and CMO? As the CEO you now have two business units reporting to your, and each will have goals that are only a slice of what you care about. You don’t care about just getting more leads, you care about those leads ending up generating revenue for the company.

Similarly with splitting out Customer Success from the CTO role (typically instead put under the CCO). The Customer Success team optimizes for solving incoming cases. Great! But why are they getting those cases? The true goal is to minimize the amount of incoming cases in the first place, and now you need to have another business unit, the CTO/CTPO, work towards another units’ goals. Instead, the focus should be on quality of the Product, and the goal should be to optimize Customer Success  by minimizing the required investment into by improving the Product and Engineering quality—effectively closing the loop for the goal.

## Keeping a growing hierarchy flat

A quick glance at the Organizational structure might make it seem like we’ve created several layers. Layers like these are sometimes a necessary evil, but they don’t have to become *reporting chains*.

What you don’t want is layers in your communication hierarchy.

On the more experimental end of things, you have people like NVIDIA’s CEO, Jensen Huang, that famously has 50 direct reports[^nvidia-ceo]. You don’t have to go that extreme, there’s a middle-ground.

Instead of the CEO only gathering their executives (i.e. the C-suite) in strategic meetings, you can expand this to include the leaders in the layer reporting into your C-suite as well.

This has a few benefits:

- The CEO is not overloaded with direct reports, leaving that to the CFO, CCO, and CTO/CTPO
- The executive team can quickly disseminate knowledge to the company leadership, which importantly includes the leaders reporting to the C-suite
- You avoid a word-of-mouth spreading of change, values, problem solving, etc - often I’ve seen things being decided in the executive team, and then being communicated very differently down into each organization, causing very different views, repeated conversations, and has many downsides to it
- You create a much stronger sense of ownership and involvement from your leaders in your organization - remember that the leaders on the floor/frontline (probably not your C-suite) are the ones that will be effectuating *actual* change in your organization

We are essentially trying to combat the exponential growth that an increase in communication channels have, but flattening our communication layer, but not necessarily our reporting layer.

[^nvidia-ceo]: [https://www.cnbc.com/2023/11/29/nvidia-ceo-senior-executives-dont-need-pampering-career-guidance.html](https://www.cnbc.com/2023/11/29/nvidia-ceo-senior-executives-dont-need-pampering-career-guidance.html)

## Summary

This is a very opinionated look on an ideal high-level organization structure, for a Product company. There are always nuances that will change given the context you operate in, but it should hopefully at least have provoked some thoughts into how and especially *why* you have structured your organization the way you have.

An important aspect that might change what you end up with is the competences of the people you get onboard. You might not have a CTO with a strong Product sense for example. While that might be a bit of a problem in its own way, there are many ways you could have ended up there.

The important thing to look at then is: How can you strengthen this leaders’ weak areas? That’s ultimately the responsibility of the CEO, and a way to do that is to guide the CTO to hire a strong Head/VP of Product that will report to them and help them grow their expertise in the domain.

One thing you cannot skimp on though is the leadership skills of your executive team. If they are not where they should be, then either the CEO needs to mentor and coach them, or you could get external coaching for the CTO to have someone to spar with.

The same goes of course for the other roles.

## Further Reading

Harry Glaser recently posted an excellent article on how to run an exec team, which also goes into a lot of the organizational aspects around this. It takes a bit more of a standard look on how to structure the organization, but manages to not be too prescriptive.

You can read his post here [https://www.harryglaser.com/how-to-build-and-run-your-exec-team/](https://www.harryglaser.com/how-to-build-and-run-your-exec-team/).

{{ medium_comments(post="structuring-your-engineering-organization-2818dc645b09") }}

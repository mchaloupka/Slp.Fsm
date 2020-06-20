# Slp.Fsm

---

## What is Slp.Fsm?

Slp.Fsm is a library to create and manipulate Finite State automaton.

## Why use Slp.Fsm?

I have created this package as I have not found a similar one. My goal was related to [R2RML](https://www.w3.org/TR/r2rml/) processing. I wanted to answer a question whether two templates can generate the same value. Usage of finite state machine seems to be a perfect fit. I want to build a machine for both templates, do intersection of them. Output of that is a machine that accepts only strings that can be produced by both templates.

I have looked for other finite state machine implementations but I had no luck with it. I had some specific requirements - I wanted to define custom edges (like an edge accepting all IRI unreserved characters). I have not found any solution matching my needs so I decided to implement a simple one by myself.

---

<div class="row row-cols-1 row-cols-md-2">
  <div class="col mb-4">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Tutorials</h5>
        <p class="card-text">Takes you by the hand through a series of steps to create your first automaton. </p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Tutorials/index.html" class="btn btn-primary">Get started</a>
      </div>
    </div>
  </div>
  <div class="col mb-4">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">How-To Guides</h5>
        <p class="card-text">Guides you through the steps involved in various use-cases. </p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/How_Tos/index.html" class="btn btn-primary">Learn Usecases</a>
      </div>
    </div>
  </div>
  <div class="col mb-4 mb-md-0">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Explanations</h5>
        <p class="card-text">Discusses key topics and concepts at a fairly high level and provide useful background information and explanation.</p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Explanations/index.html" class="btn btn-primary">Dive Deeper</a>
      </div>
    </div>
  </div>
  <div class="col">
    <div class="card h-100">
      <div class="card-body">
        <h5 class="card-title">Api Reference</h5>
        <p class="card-text">Contain technical reference for APIs.</p>
      </div>
      <div class="card-footer text-right   border-top-0">
        <a href="{{siteBaseUrl}}/Api_Reference/index.html" class="btn btn-primary">Read Api Docs</a>
      </div>
    </div>
  </div>
</div>

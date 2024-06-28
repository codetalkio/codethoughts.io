window.addEventListener("DOMContentLoaded", () => {
  function isInView(elem) {
    const boundingRect = elem.getBoundingClientRect();

    if (
      boundingRect.top >= 0 &&
      boundingRect.left >= 0 &&
      boundingRect.bottom <=
        (window.innerHeight || document.documentElement.clientHeight) &&
      boundingRect.right <=
        (window.innerWidth || document.documentElement.clientWidth)
    ) {
      return true;
    } else {
      return false;
    }
  }

  let prevScrollTop = window.pageYOffset || document.documentElement.scrollTop;
  let prevScrollDirection = "";
  window.addEventListener(
    "scroll",
    function () {
      const st = window.pageYOffset || document.documentElement.scrollTop;
      if (st > prevScrollTop && prevScrollDirection !== "down") {
        prevScrollDirection = "down";
      } else if (st < prevScrollTop && prevScrollDirection !== "up") {
        prevScrollDirection = "up";
      }
      prevScrollTop = st <= 0 ? 0 : st; // for Mobile or negative scrolling
    },
    false
  );

  function isNewestInScrollDirection(elem1, elem2) {
    const boundingRect1 = elem1.getBoundingClientRect();
    const boundingRect2 = elem2.getBoundingClientRect();

    if (
      prevScrollDirection == "down" &&
      boundingRect1.top > boundingRect2.top
    ) {
      return true;
    } else if (
      prevScrollDirection == "up" &&
      boundingRect1.top < boundingRect2.top
    ) {
      return true;
    } else {
      return false;
    }
  }

  // During long sections the heading may no longer be showing, but we still
  // want to indicate to the user where they are, so we hang on to it.
  let lastTargets = [];
  let lastToBeAdded = null;
  const observer = new IntersectionObserver((entries) => {
    entries.forEach((entry) => {
      const id = entry.target.getAttribute("id");
      const tocItem = document.querySelector(
        `#table-of-contents-sidebar a[href$="#${id}"]`
      );
      if (tocItem) {
        if (entry.intersectionRatio > 0) {
          const newTargets = [entry.target];
          lastTargets.forEach((lastTarget) => {
            if (!isInView(lastTarget)) {
              const lastTargetId = lastTarget.getAttribute("id");
              const lastTargetTocItem = document.querySelector(
                `#table-of-contents-sidebar a[href$="#${lastTargetId}"]`
              );
              lastTargetTocItem.parentElement.classList.remove(
                "active-toc-sidebar-item"
              );
            } else {
              newTargets.push(lastTarget);
            }
          });

          lastTargets = newTargets;
          if (
            lastToBeAdded == null ||
            !isInView(lastToBeAdded) ||
            (isInView(lastToBeAdded) &&
              isNewestInScrollDirection(entry.target, lastToBeAdded))
          ) {
            lastToBeAdded = entry.target;
          }

          tocItem.parentElement.classList.add("active-toc-sidebar-item");
        } else if (lastTargets.length > 0) {
          lastTargets.forEach((lastTarget) => {
            if (lastTarget != lastToBeAdded && !isInView(lastTarget)) {
              const lastTargetId = lastTarget.getAttribute("id");
              const lastTargetTocItem = document.querySelector(
                `#table-of-contents-sidebar a[href$="#${lastTargetId}"]`
              );
              lastTargetTocItem.parentElement.classList.remove(
                "active-toc-sidebar-item"
              );
            }
          });
        }
      }
    });
  });

  // Track all h1-4 that have an id on them.
  document.querySelectorAll("h1[id]").forEach((item) => {
    observer.observe(item);
  });
  document.querySelectorAll("h2[id]").forEach((item) => {
    observer.observe(item);
  });
  document.querySelectorAll("h3[id]").forEach((item) => {
    observer.observe(item);
  });
  document.querySelectorAll("h4[id]").forEach((item) => {
    observer.observe(item);
  });
});

/**
 * visibleInViewport Directive [Author: Abdul Ashfaque M]
 * Angular directive for emiting a boolean when a components or an elements come close to the viewport.
 * The directive uses the Intersection Observer API to detect when an element enters the viewport with a specified offset.
 * It emits a boolean value indicating whether the element is viewed for the first time or has already been viewed.
 *
 * @usage
 * Import VisibleInViewportDirective in your module.
 * Use the `[appVisibleInViewport]` selector in your template.
 * Bind the `enableRepeatedLoad` input to specify if the element should enable repeated visibleInViewport.
 * Utilize the `(visibleInViewport)` output to handle the loading logic for your component or element.
 *  ```html
 *  <!-- app.component.html -->
 *  <div *ngIf="componentVisible">
 *     <app-your-lazy-loaded-component></app-your-lazy-loaded-component>
 *  </div>
 *
 *  <div appVisibleInViewport [enableRepeatedLoad]="false" (visibleInViewport)="onvisibleInViewport($event)">
 *     <!-- This is the place where the lazy loading is triggered -->
 *     Loading...
 *  </div>
 *  ```
 *
 * @selector
 * Selector: '[appVisibleInViewport]'
 *
 * @implements
 * OnInit: Initializes the directive and sets up the Intersection Observer.
 * OnChanges: to update the observer when there is a change in isViewportVisiblityEnabled.
 * OnDestroy: Cleans up the Intersection Observer when the directive is destroyed.
 *
 * @input
 * @property {number} minHeight - Minimum height of the element to ensure it is properly detected. Default is 100.
 * @property {boolean} isViewportVisiblityEnabled - Boolean indicating whether lazy loading is enabled. Default is true.
 * @property {boolean} enableRepeatedLoad - Boolean indicating whether the element should load multiple times. Default is true.
 * @property {boolean} enableRepeatedLoad - Boolean indicating whether the element should load multiple times. Default is true.
 * @property {number} intialDelayTimeToObserve - Initial delay time before the observer starts observing. Default is 0.
 * @property {IntersectionObserverInit} intersectionObserverOptions - Options for the Intersection Observer. Default is `{ root: null, rootMargin: '0px 0px 0px 0px', threshold: 0.01 }`.
 *   - `root`: The element that is used as the viewport for checking visibility (defaults to the browser viewport if null).
 *   - `rootMargin`: Margin around the root element to expand or contract the viewport area (specified in pixels or percentages).
 *   - `threshold`: A single number or array of numbers indicating at what percentage of the target's visibility the observer callback should be executed.
 *
 * @output
 * visibleInViewport: Event emitter that emits a true value when the element comes into the viewport.
 *
 * @methods
 * createObserver(): create the Intersection observer.
 * disconnectObserver(): to destroy the Intersection observer.
 * updateObserver(): Manages the observer based on the current state of isViewportVisiblityEnabled.
 *
 * @remarks
 * The directive supports lazy loading with a customizable offset and can be configured to emit boolean single or multiple times.
 * It uses the Intersection Observer API to efficiently observe changes in the visibility of the target element.
 * 
 * @issue
 * need to make set intersectionObserverOptions dynamically while changing it.
 */

import { Directive, ElementRef, EventEmitter, Input, OnDestroy, OnInit, Output, SimpleChanges } from '@angular/core';

@Directive({
    selector: '[appVisibleInViewport]'
})
export class VisibleInViewportDirective implements OnInit, OnDestroy {

    @Input() minHeight = 100;
    @Input() enableRepeatedLoad = false;
    @Input() intialDelayTimeToObserve = 0;
    @Input() isViewportVisiblityEnabled = true;
    @Input() intersectionObserverOptions: IntersectionObserverInit = {
        root: null,
        rootMargin: '0px 0px 0px 0px',
        threshold: 0.01
    };

    @Output() visibleInViewport = new EventEmitter<boolean>();

    private hasVisibleInViewport = false;
    private isOberverDisconnected = false;
    private observer: IntersectionObserver;

    constructor(private el: ElementRef) { }

    ngOnInit(): void {
        this.el.nativeElement.style.minHeight = this.minHeight + 'px';
        this.createObserver();
    }

    ngOnChanges(changes: SimpleChanges): void {
        if (changes.isViewportVisiblityEnabled) {
            this.updateObserver();
        }
    }

    private createObserver(): void {
        this.observer = new IntersectionObserver((observedEntries: IntersectionObserverEntry[]) => {
            observedEntries.forEach(entry => {
                if (entry.isIntersecting) {
                    this.hasVisibleInViewport = true;
                    this.visibleInViewport.emit(this.hasVisibleInViewport);
                    if (!this.enableRepeatedLoad) {
                        this.disconnectObserver();
                    }
                }
            });
        }, this.intersectionObserverOptions);

        setTimeout(() => {
            this.updateObserver(); // Call method to start/stop lazy loading
        }, this.intialDelayTimeToObserve);
    }

    private updateObserver(): void {
        if (!this.isOberverDisconnected) {
            this.isViewportVisiblityEnabled ? this.observer?.observe(this.el.nativeElement) : this.observer?.unobserve(this.el.nativeElement);
        }
    }

    private disconnectObserver(): void {
        if (this.observer) {
            this.observer.disconnect();
            this.isOberverDisconnected = true;
        }
    }

    ngOnDestroy(): void {
        this.disconnectObserver();
    }
}

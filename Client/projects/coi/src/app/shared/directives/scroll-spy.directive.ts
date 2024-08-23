import { Directive, Input, OnChanges, SimpleChanges, ElementRef, OnInit, OnDestroy, EventEmitter, Output } from '@angular/core';

@Directive({
  selector: '[appVirtualScroll]'
})
export class VirtualScrollDirective implements OnInit, OnChanges, OnDestroy {
  @Input() scrollElementId!: string;
  @Input() items: any[] = [];
  @Input() itemHeight: number = 50; // Height of each item
  @Input() buffer: number = 5; // Number of items to buffer
  @Output() visibleItems = new EventEmitter<any[]>();

  private container!: HTMLElement;
  private startIndex: number = 0;
  private endIndex: number = 0;
  private visibleItemsList: any[] = [];
  private scrollHandler: () => void;

  private topDummyDiv!: HTMLDivElement;
  private bottomDummyDiv!: HTMLDivElement;

  constructor(private el: ElementRef) {}

  ngOnInit(): void {
    this.initializeContainer();
    if (this.container) {
      this.createDummyDivs();
      this.updateVisibleItems();
      this.scrollHandler = () => this.updateVisibleItems();
      this.container.addEventListener('scroll', this.scrollHandler);
    }
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.items || changes.itemHeight) {
      this.updateDummyDivs();
      this.updateVisibleItems();
    }
  }

  ngOnDestroy(): void {
    if (this.container) {
      this.container.removeEventListener('scroll', this.scrollHandler);
    }
  }

  private initializeContainer(): void {
    this.container = document.getElementById(this.scrollElementId) || this.el.nativeElement;
    if (!this.container) {
      console.error(`Element with ID ${this.scrollElementId} not found.`);
    }
  }

  private createDummyDivs(): void {
    this.topDummyDiv = document.createElement('div');
    this.bottomDummyDiv = document.createElement('div');
    
    this.topDummyDiv.style.height = '0px';
    this.bottomDummyDiv.style.height = '0px';

    this.container.prepend(this.topDummyDiv);
    this.container.append(this.bottomDummyDiv);
  }

  private updateDummyDivs(): void {
    const totalHeight = this.items.length * this.itemHeight;
    this.topDummyDiv.style.height = `${this.startIndex * this.itemHeight}px`;
    this.bottomDummyDiv.style.height = `${totalHeight - ((this.endIndex + 1) * this.itemHeight)}px`;
  }

  private updateVisibleItems(): void {
    if (this.container) {
      const scrollTop = this.container.scrollTop;
      const viewportHeight = this.container.clientHeight;

      // Calculate start and end indices
      this.startIndex = Math.max(0, Math.floor(scrollTop / this.itemHeight) - this.buffer);
      this.endIndex = Math.min(this.items.length - 1, Math.ceil((scrollTop + viewportHeight) / this.itemHeight) + this.buffer);

      // Ensure at least 5 items are shown initially
      if (this.startIndex === 0 && this.endIndex > 5) {
        this.endIndex = 5;
      }

      // Ensure the start index does not become negative
      this.startIndex = Math.max(this.startIndex, 0);

      // Load more items if we are close to the end
      if (this.endIndex === this.items.length - 1 && this.startIndex + this.buffer < this.items.length) {
        this.endIndex = Math.min(this.items.length - 1, this.startIndex + this.buffer);
      }

      this.updateDummyDivs();

      this.visibleItemsList = this.items.slice(this.startIndex, this.endIndex + 1);
      this.visibleItems.emit(this.visibleItemsList);
    }
  }
}

export class ScrollSpyConfiguration {
    activeCounter = 0;
    navItemClass = '';
    contentItemClass = '';
    isActiveKeyNavigation = false;
    focusedSection: 'LEFT' | 'RIGHT' = 'LEFT';
    offsetHeight = 0;
    leftOffsetTop = 0;
    rightOffsetTop = 0;
    leftOffsetBottom = 0;
    rightOffsetBottom = 0;
    scrollLeftHeight = 'auto';
    scrollRightHeight = 'auto';
    elementVisiblePercentageList: any[] = [];
}

export interface ScrollSpyEvent {
    isScrolling: boolean;
    navItemClass: string;
    activeCounter: number;
    previousCounter: number | null;
    contentItemClass: string;
    isActiveKeyNavigation: boolean;
    activeElement: HTMLElement | null;
    focusedSection: 'LEFT' | 'RIGHT';
}

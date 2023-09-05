import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { Observable, Subscription } from 'rxjs';
class Position {
    clientX: number;
    clientY: number;
    popoverHeight: number;
    popoverWidth: number;
}

@Component({
    selector: 'app-dynamic-popover',
    templateUrl: './dynamic-popover.component.html',
    styleUrls: ['./dynamic-popover.component.scss']
})
export class DynamicPopoverComponent implements OnInit, OnDestroy {

    constructor() { }

    @Input() showPopupEvent: Observable<boolean>;
    @Input() positionDetails = new Position();
    subscription: Subscription;

    ngOnInit() {
        this.showPopUpEventHandler();
    }

    ngOnDestroy() {
        if (this.subscription) { this.subscription.unsubscribe(); }
    }

    private showPopUpEventHandler(): void {
        this.subscription = this.showPopupEvent
            .subscribe((data: boolean) => data ? this.showBasicDetailsPopup(this.positionDetails) : this.hideBasicDetailsPopup());
    }

    private showBasicDetailsPopup(event: Position): void {
        document.body.style.overflowY = 'hidden';
        const POPUP: HTMLElement = document.querySelector('#dynamic-popover');
        POPUP.style.left = this.getLeftPosition();
        POPUP.style.top = this.getTopPosition();
        POPUP.style.display = 'block';
        POPUP.style.zIndex = '21234';
    }

    private hideBasicDetailsPopup(): void {
        document.body.style.overflowY = 'auto';
        const POPUP: HTMLElement = document.querySelector('#dynamic-popover');
        POPUP.style.display = 'none';
    }

    private getLeftPosition(): string {
        return screen.width > this.positionDetails.popoverWidth + this.positionDetails.clientX ?
            this.positionDetails.clientX  + window.scrollX + 'px' :
            this.positionDetails.clientX - this.positionDetails.popoverWidth +  window.scrollX + 'px';
    }

    private getTopPosition(): string {
        console.log(this.positionDetails.clientY);
        return screen.height > this.positionDetails.popoverHeight + this.positionDetails.clientY  ?
            this.positionDetails.clientY + window.scrollY + 'px' :
            this.positionDetails.clientY - this.positionDetails.popoverHeight + window.scrollY + 'px';
    }

}

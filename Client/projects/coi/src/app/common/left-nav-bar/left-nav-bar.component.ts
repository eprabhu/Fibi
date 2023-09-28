import {Component, ElementRef, ViewChild} from '@angular/core';
import {environment} from '../../../environments/environment';

@Component({
    selector: 'app-left-nav-bar',
    templateUrl: './left-nav-bar.component.html',
    styleUrls: ['./left-nav-bar.component.scss']
})
export class LeftNavBarComponent {
    deployMap = environment.deployUrl;
    isNavExpanded = false;
    @ViewChild('sideBarMenu', {static: true}) sideBarMenu: ElementRef;

    constructor() {
        document.addEventListener('mouseup', this.offClickSideBarHandler.bind(this));
    }

    offClickSideBarHandler(event) {
        if (this.isNavExpanded) {
            this.toggleNavBar(!this.isNavExpanded);
        }
    }

    toggleNavBar(quickClose = false) {
        if (quickClose) {
            this.isNavExpanded = !this.isNavExpanded;
        } else {
            setTimeout(() => { this.isNavExpanded = !this.isNavExpanded; }, 200);
        }
    }
}

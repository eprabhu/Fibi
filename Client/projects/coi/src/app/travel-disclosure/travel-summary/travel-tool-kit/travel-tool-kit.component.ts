import { Component } from '@angular/core';

@Component({
    selector: 'app-travel-tool-kit',
    templateUrl: './travel-tool-kit.component.html',
    styleUrls: ['./travel-tool-kit.component.scss'],
})
export class TravelToolKitComponent {

    activeNav: string = '';

    jumpToSection(section: string): void {
        this.activeNav = section;
        const SCROLL_SECTION = document.getElementById(section).offsetTop - 400;
        window.scrollTo({ top: SCROLL_SECTION, behavior: 'smooth' });
    }

}

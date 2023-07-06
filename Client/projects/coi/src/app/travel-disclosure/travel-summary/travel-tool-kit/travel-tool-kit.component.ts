import { Component, OnInit } from '@angular/core';
import { slideHorizontal } from 'projects/fibi/src/app/common/utilities/animations';

@Component({
    selector: 'app-travel-tool-kit',
    templateUrl: './travel-tool-kit.component.html',
    styleUrls: ['./travel-tool-kit.component.scss'],
    animations: [slideHorizontal]
})
export class TravelToolKitComponent implements OnInit {

    constructor() { }

    ngOnInit() {
    }

    jumpToSection(section) {
        const sectionHeight = document.getElementById(section).offsetTop - 270;
        window.scrollTo({ top: sectionHeight, behavior: 'smooth' });
    }


}

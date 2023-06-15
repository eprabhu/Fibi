import { Component, OnInit } from '@angular/core';
import { slideHorizontal } from 'projects/fibi/src/app/common/utilities/animations';
import { scrollIntoView } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-travel-tool-kit',
    templateUrl: './travel-tool-kit.component.html',
    styleUrls: ['./travel-tool-kit.component.scss'],
    animations: [slideHorizontal]
})
export class TravelToolKitComponent implements OnInit {

    scrollIntoView = scrollIntoView;
    constructor() { }

    ngOnInit() {
    }

}

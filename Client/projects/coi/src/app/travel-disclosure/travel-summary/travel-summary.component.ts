import { Component, OnInit } from '@angular/core';
import { fadeInOutHeight } from '../../common/utilities/animations';

@Component({
    selector: 'app-travel-summary',
    templateUrl: './travel-summary.component.html',
    styleUrls: ['./travel-summary.component.scss'],
    animations: [fadeInOutHeight]
})
export class TravelSummaryComponent implements OnInit {

    ngOnInit(): void {
        window.scrollTo(0, 0);
    }

}

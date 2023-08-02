import {Component, OnInit} from '@angular/core';
import { fadeInOutHeight } from '../../common/utilities/animations';

@Component({
    selector: 'app-review',
    templateUrl: './review.component.html',
    styleUrls: ['./review.component.css'],
    animations: [fadeInOutHeight]
})
export class ReviewComponent implements OnInit {

    constructor() { }

    ngOnInit() { }

}

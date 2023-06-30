import { Component, Input } from '@angular/core';

@Component({
    selector: 'app-person-details',
    templateUrl: './person-details.component.html',
    styleUrls: ['./person-details.component.scss']
})
export class PersonDetailsComponent {
    @Input() personValues: any;
    constructor() { }
}

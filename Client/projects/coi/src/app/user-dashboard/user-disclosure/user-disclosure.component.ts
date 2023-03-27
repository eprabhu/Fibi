import {Component} from '@angular/core';

@Component({
    selector: 'app-user-disclosure',
    templateUrl: './user-disclosure.component.html',
    styleUrls: ['./user-disclosure.component.scss']
})
export class UserDisclosureComponent {
    currentSelected = {
        tab: 'IN_PROGRESS',
        filter: 'ALL'
    }
}

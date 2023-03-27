import {Component} from '@angular/core';

@Component({
    selector: 'app-user-entities',
    templateUrl: './user-entities.component.html',
    styleUrls: ['./user-entities.component.scss']
})
export class UserEntitiesComponent {
    currentSelected = {
        tab: 'IN_PROGRESS',
        filter: 'ALL'
    }
}

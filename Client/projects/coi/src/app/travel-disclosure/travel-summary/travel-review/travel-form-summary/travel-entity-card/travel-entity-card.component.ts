import { Component, Input, OnInit } from '@angular/core';
import { EntityData } from '../../../../travel-disclosure-interface';
import { Router } from '@angular/router';

@Component({
    selector: 'app-travel-entity-card',
    templateUrl: './travel-entity-card.component.html',
    styleUrls: ['./travel-entity-card.component.scss']
})
export class TravelEntityCardComponent implements OnInit {

    @Input() entityData: EntityData;

    constructor(private _router: Router) { }

    ngOnInit() {
    }

    viewDetails(entityId: number) {
        this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: entityId } });
    }
}

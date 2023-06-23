import { Component, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { TravelDataStoreService } from '../../../services/travel-data-store.service';
import { TravelDisclosureService } from '../../../services/travel-disclosure.service';
import { EntityData, TravelDisclosureResponseObject, TravelDisclosureTraveller } from '../../../travel-disclosure-interface';

@Component({
    selector: 'app-travel-form-summary',
    templateUrl: './travel-form-summary.component.html',
    styleUrls: ['./travel-form-summary.component.scss']
})
export class TravelFormSummaryComponent implements OnInit {

    isCollapsed = true;
    $subscriptions: Subscription[] = [];
    travelDisclosureData = new TravelDisclosureResponseObject();
    travellerTypeLookup: Array<TravelDisclosureTraveller>;
    traveller = '';
    isReadMorePurpose = false;
    isReadMoreRelation = false;
    entityData = new EntityData();
    travellerTypeCodeList = [];

    constructor(private _dataStore: TravelDataStoreService, private _service: TravelDisclosureService) { }

    ngOnInit(): void {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    private getDataFromStore(): void {
        this.travelDisclosureData = this._dataStore.getData();
        this.setEntityData();
        this.setTravellerType();
    }

    private setEntityData(): void {
        this.entityData.country = this.travelDisclosureData.country;
        this.entityData.entityId = this.travelDisclosureData.entityId;
        this.entityData.entityName = this.travelDisclosureData.travelEntityName;
        this.entityData.entityType = this.travelDisclosureData.entityType;
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private setTravellerType(): void {
        if (this.travelDisclosureData.travellerTypeCodeList) {
            Object.keys(this.travelDisclosureData.travellerTypeCodeList).forEach((typeCode: any) => {
                const travellerType = this.travelDisclosureData.travellerTypeCodeList[typeCode];
                this.traveller += this.traveller === '' ? travellerType : `, ${travellerType}`;
            });
        }
    }
}

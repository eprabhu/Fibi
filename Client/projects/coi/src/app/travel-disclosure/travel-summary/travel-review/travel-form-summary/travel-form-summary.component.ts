import { Component, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { TravelDataStoreService } from '../../../services/travel-data-store.service';
import { TravelDisclosureService } from '../../../services/travel-disclosure.service';
import { EntityDetails, TravelDisclosureResponseObject, TravelDisclosureTraveller } from '../../../travel-disclosure-interface';

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
    entityDetails = new EntityDetails();

    constructor(private _dataStore: TravelDataStoreService, private _service: TravelDisclosureService) { }

    ngOnInit(): void {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    private getDataFromStore(): void {
        this.travelDisclosureData = this._dataStore.getData();
        this.setEntityDetails();
        this.loadTravellerTypesLookup();
    }

    private setEntityDetails(): void {
        this.entityDetails.countryName = null;
        this.entityDetails.entityId = this.travelDisclosureData.entityId || null;
        this.entityDetails.entityName = this.travelDisclosureData.travelEntityName || null;
        this.entityDetails.entityNumber = this.travelDisclosureData.entityNumber || null;
        this.entityDetails.involvementEndDate = this.travelDisclosureData.travelEndDate || null;
        this.entityDetails.involvementStartDate = this.travelDisclosureData.travelStartDate || null;
        this.entityDetails.isActive = true;
        this.entityDetails.entityType = null;
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private loadTravellerTypesLookup(): void {
        this.$subscriptions.push(this._service.loadTravellerTypesLookup()
            .subscribe((data: any) => {
                if (data) {
                    this.travellerTypeLookup = data;
                    this.setTravellerType();
                }
            }));
    }

    private setTravellerType(): void {
        if (this.travelDisclosureData.travellerTypeCodeList.length > 0) {
            for (const type of this.travelDisclosureData.travellerTypeCodeList) {
                const matchingDetail = this.travellerTypeLookup.find(details => details.travelerTypeCode === type);
                if (matchingDetail) {
                    this.traveller += this.traveller === '' ? matchingDetail.description : `, ${matchingDetail.description}`;
                }
            }
        }
    }
}

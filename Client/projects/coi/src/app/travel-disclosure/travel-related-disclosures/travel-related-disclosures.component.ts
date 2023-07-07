import { Component, OnDestroy, OnInit } from '@angular/core';
import { TravelDataStoreService } from '../services/travel-data-store.service';
import { TravelDisclosureResponseObject, EntityData, TravelHistoryRO, TravelHistory } from '../travel-disclosure-interface';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { TravelDisclosureService } from '../services/travel-disclosure.service';
import { HTTP_ERROR_STATUS, POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { Router } from '@angular/router';
import { getFormattedAmount } from '../../common/utlities/custom-utlities';
import { listAnimation } from 'projects/fibi/src/app/common/utilities/animations';

@Component({
    selector: 'app-travel-related-disclosures',
    templateUrl: './travel-related-disclosures.component.html',
    styleUrls: ['./travel-related-disclosures.component.scss'],
    animations: [listAnimation]
})
export class TravelRelatedDisclosureComponent implements OnInit, OnDestroy {

    historyData: Array<TravelHistory> = [];
    travelDisclosureData = new TravelDisclosureResponseObject();
    entityData = new EntityData();
    $subscriptions: Subscription[] = [];

    constructor(
        private _dataStore: TravelDataStoreService,
        private _service: TravelDisclosureService,
        private _commonService: CommonService,
        private _router: Router
    ) { }

    ngOnInit(): void {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore(): void {
        this.travelDisclosureData = this._dataStore.getData();
        this.setEntityData();
        this.loadTravelDisclosureHistory();
    }

    private setEntityData(): void {
        this.entityData.country = this.travelDisclosureData.country;
        this.entityData.entityId = this.travelDisclosureData.entityId;
        this.entityData.entityType = this.travelDisclosureData.entityType;
        this.entityData.entityName = this.travelDisclosureData.travelEntityName;

    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getTravelHistoryRO(): TravelHistoryRO {
        return {
            'personId': this.travelDisclosureData.personId,
            'entityNumber': this.travelDisclosureData.entityNumber
        };
    }

    private loadTravelDisclosureHistory(): void {
        this._service.loadTravelDisclosureHistory(this.getTravelHistoryRO())
            .subscribe((res: Array<TravelHistory>) => {
                if (res) {
                    this.historyData = res;
                }
            }, (err) => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in Loading Travel Disclosure History');
            });
    }

    getTravellerType(): string {
        let traveller = '';
        if (this.travelDisclosureData.travellerTypeCodeList) {
            Object.keys(this.travelDisclosureData.travellerTypeCodeList).forEach((typeCode: any) => {
                const travellerType = this.travelDisclosureData.travellerTypeCodeList[typeCode];
                traveller += traveller === '' ? travellerType : `, ${travellerType}`;
            });
        }
        return traveller;
    }

    getFormattedAmount(travelAmount: number): string {
        return getFormattedAmount(travelAmount);
    }

    viewTravelDisclosure(travelDisclosureId: number) {
        const url = '/#/' + this._router.createUrlTree([POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL],
            { queryParams: { disclosureId: travelDisclosureId } }).toString();
        window.open(url, '_blank');
    }
}

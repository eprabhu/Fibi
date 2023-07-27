import { Component, OnDestroy, OnInit } from '@angular/core';
import { TravelDataStoreService } from '../services/travel-data-store.service';
import { TravelDisclosure, EntityData, TravelHistoryRO, TravelHistory } from '../travel-disclosure-interface';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { TravelDisclosureService } from '../services/travel-disclosure.service';
import { HTTP_ERROR_STATUS, POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { Router } from '@angular/router';
import { getFormattedAmount } from '../../common/utlities/custom-utlities';
import { fadeInOutHeight, listAnimation } from '../../../../../fibi/src/app/common/utilities/animations';

@Component({
    selector: 'app-travel-related-disclosures',
    templateUrl: './travel-related-disclosures.component.html',
    styleUrls: ['./travel-related-disclosures.component.scss'],
    animations: [listAnimation, fadeInOutHeight]
})
export class TravelRelatedDisclosureComponent implements OnInit, OnDestroy {

    isLoading = false;
    historyData: Array<TravelHistory> = [];
    travelDisclosure = new TravelDisclosure();
    entityData: EntityData = new EntityData();
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
        this.travelDisclosure = this._dataStore.getData();
        this.setEntityData();
        this.loadTravelDisclosureHistory();
    }

    private setEntityData(): void {
        this.entityData = {
            isActive: this.travelDisclosure.entityIsActive,
            country: { countryName: this.travelDisclosure.country },
            entityId: this.travelDisclosure.entityId,
            entityType: { description: this.travelDisclosure.entityType },
            entityName: this.travelDisclosure.travelEntityName,
            emailAddress: this.travelDisclosure.entityEmail,
            address: this.travelDisclosure.entityAddress
        };
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
            'personId': this.travelDisclosure.personId,
            'entityNumber': this.travelDisclosure.entityNumber
        };
    }

    private loadTravelDisclosureHistory(): void {
        this.isLoading = true;
        this.$subscriptions.push(
            this._service.loadTravelDisclosureHistory(this.getTravelHistoryRO())
                .subscribe((res: Array<TravelHistory>) => {
                    if (res) {
                        this.isLoading = false;
                        this.historyData = res;
                    }
                }, (err) => {
                    this.isLoading = false;
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in Loading Travel Disclosure History');
                })
        );
    }

    getTravellerType(): string {
        let traveller = '';
        if (this.travelDisclosure.travellerTypeCodeList) {
            Object.keys(this.travelDisclosure.travellerTypeCodeList).forEach((typeCode: any) => {
                const travellerType = this.travelDisclosure.travellerTypeCodeList[typeCode];
                traveller += traveller === '' ? travellerType : `, ${travellerType}`;
            });
        }
        return traveller;
    }

    viewEntity(entityId: string): void {
        this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: entityId } });
    }

    getFormattedAmount(travelAmount: number): string {
        return getFormattedAmount(travelAmount);
    }

    viewTravelDisclosure(travelDisclosureId: number) {
        const url = '/#' + this._router.createUrlTree([POST_CREATE_TRAVEL_DISCLOSURE_ROUTE_URL],
            { queryParams: { disclosureId: travelDisclosureId } }).toString();
        window.open(url, '_blank');
    }
}

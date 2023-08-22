import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { TravelDataStoreService } from '../../../services/travel-data-store.service';
import { EntityDetails, TravelDisclosure, TravelDisclosureTraveller } from '../../../travel-disclosure-interface';
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { getFormattedAmount } from '../../../../common/utilities/custom-utilities';
import { Router } from '@angular/router';

@Component({
    selector: 'app-travel-form-summary',
    templateUrl: './travel-form-summary.component.html',
    styleUrls: ['./travel-form-summary.component.scss']
})
export class TravelFormSummaryComponent implements OnInit, OnDestroy {

    isCollapsed = true;
    $subscriptions: Subscription[] = [];
    travelDisclosure = new TravelDisclosure();
    travellerTypeLookup: Array<TravelDisclosureTraveller>;
    traveller = '';
    isReadMorePurpose = false;
    isReadMoreRelation = false;
    entityDetails: EntityDetails = new EntityDetails();
    travellerTypeCodeList = [];

    constructor(private _dataStore: TravelDataStoreService, private _router: Router) { }

    ngOnInit(): void {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore(): void {
        this.travelDisclosure = this._dataStore.getData();
        this.entityDetails = this._dataStore.getEntityDetails();
        this.setTravellerType();
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private setTravellerType(): void {
        this.traveller = '';
        if (this.travelDisclosure.travellerTypeCodeList) {
            Object.keys(this.travelDisclosure.travellerTypeCodeList).forEach((typeCode: any) => {
                const travellerType = this.travelDisclosure.travellerTypeCodeList[typeCode];
                this.traveller += this.traveller === '' ? travellerType : `, ${travellerType}`;
            });
        }
    }

    viewEntity(entityId: string): void {
        this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: entityId } });
    }

    getFormattedAmount(travelAmount: number): string {
        return getFormattedAmount(travelAmount);
    }
}

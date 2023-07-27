import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { TravelDataStoreService } from '../../../services/travel-data-store.service';
import { EntityData, TravelDisclosure, TravelDisclosureTraveller } from '../../../travel-disclosure-interface';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { getFormattedAmount } from 'projects/coi/src/app/common/utlities/custom-utlities';
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
    entityData: EntityData = new EntityData();
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
        this.setEntityData();
        this.setTravellerType();
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

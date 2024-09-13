import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { EntityManagementService } from '../../entity-management.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { Subscription } from 'rxjs';
import { EntityCardDetails } from '../entity-interface';
import { Router } from '@angular/router';
import { isEmptyObject, openInNewTab } from '../../../common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';

export class DunsMatchDetails {
    matchedDunsNumber: string;
    isDunsMatched: boolean;
}
@Component({
    selector: 'app-entity-common-card',
    templateUrl: './entity-common-card.component.html',
    styleUrls: ['./entity-common-card.component.scss']
})
export class EntityCommonCardComponent implements OnInit, OnDestroy {
    isExpanded = false;
    $subscriptions: Subscription[] = [];
    dunsMatchDetails = new DunsMatchDetails();
    isDunsMatchAlreadyDone = false;

    @Input() detailsSource: 'DUNS' | 'LOCAL' = 'LOCAL';
    @Input() entityDetailsObj = new EntityCardDetails();
    @Output() emitCardNextAction = new EventEmitter<'USE' | 'OPEN_MODAL'>();

    constructor(public entityManagementService: EntityManagementService,
        private _dataStorService: EntityDataStoreService, private _router: Router) { }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        if (this.detailsSource === 'DUNS') {
            this.setMatchingPercentage();
        }
    }

    setMatchingPercentage() {
        this.entityDetailsObj.matchQualityInformation = ((this.entityDetailsObj?.matchQualityInformation) / 10) * 100;
    }

    sendEntityDetails() {
        this.emitCardNextAction.emit('USE');
    }

    private getDataFromStore() {
        const ENTITY_DATA = this._dataStorService.getData();
        if (ENTITY_DATA && !isEmptyObject(ENTITY_DATA)) {
            this.dunsMatchDetails.matchedDunsNumber = ENTITY_DATA?.entityDetails?.dunsNumber;
            this.dunsMatchDetails.isDunsMatched = ENTITY_DATA?.entityDetails?.isDunsMatched;
            this.isDunsMatchAlreadyDone = this.dunsMatchDetails.isDunsMatched && this.dunsMatchDetails.matchedDunsNumber == this.entityDetailsObj?.dunsNumber;
        } else {
            return;
        }
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStorService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    openEntity(): void {
        openInNewTab('manage-entity/entity-overview?', ['entityManageId'], [this.entityDetailsObj.entityId]);
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

}

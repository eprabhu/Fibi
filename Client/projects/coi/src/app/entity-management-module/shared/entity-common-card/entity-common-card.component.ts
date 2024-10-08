import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { EntityManagementService } from '../../entity-management.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { Subscription } from 'rxjs';
import { EntityCardDetails } from '../entity-interface';
import { ActivatedRoute, Router } from '@angular/router';
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
    isDuplicateEntityAvailable = false;
    isAlreadyMarkedAsDuplicate = false;

    @Input() customClass = '';
    @Input() isShowDuplicateMark = false;
    @Input() detailsSource: 'DUNS' | 'LOCAL' = 'LOCAL';
    @Input() entityDetailsObj = new EntityCardDetails();
    @Output() emitCardNextAction = new EventEmitter<'USE' | 'OPEN_MODAL'>();

    constructor(public entityManagementService: EntityManagementService,
        private _route: ActivatedRoute,
        private _dataStorService: EntityDataStoreService) { }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        if (this.detailsSource === 'DUNS') {
            this.setMatchingPercentage();
        }
        this.isDuplicateEntityAvailable = this.entityDetailsObj.duplicateEntityDetails && !isEmptyObject(this.entityDetailsObj.duplicateEntityDetails) &&
        this.entityDetailsObj.duplicateEntityDetails.entityId != this._route.snapshot.queryParamMap.get('entityManageId');
    }

    setMatchingPercentage() {
        this.entityDetailsObj.matchQualityInformation = ((this.entityDetailsObj?.matchQualityInformation) / 10) * 100;
    }

    sendEntityDetails() {
        this.emitCardNextAction.emit('USE');
    }

    openMarkAsDuplicateModal(): void {
        this.emitCardNextAction.emit('OPEN_MODAL');
    }

    private getDataFromStore() {
        const ENTITY_DATA = this._dataStorService.getData();
        if (ENTITY_DATA && !isEmptyObject(ENTITY_DATA)) {
            this.dunsMatchDetails.matchedDunsNumber = ENTITY_DATA?.entityDetails?.dunsNumber;
            this.dunsMatchDetails.isDunsMatched = ENTITY_DATA?.entityDetails?.isDunsMatched;
            this.isDunsMatchAlreadyDone = this.dunsMatchDetails.isDunsMatched && this.dunsMatchDetails.matchedDunsNumber == this.entityDetailsObj?.dunsNumber;
            this.isAlreadyMarkedAsDuplicate = ENTITY_DATA?.entityDetails?.originalEntityId == this.entityDetailsObj?.duplicateEntityDetails?.entityId;
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

    openEntity(entityId: any): void {
        openInNewTab('manage-entity/entity-overview?', ['entityManageId'], [entityId]);
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

}

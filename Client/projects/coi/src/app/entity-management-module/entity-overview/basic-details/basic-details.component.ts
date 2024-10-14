import {Component, Input, OnDestroy, OnInit} from '@angular/core';
import { DataStoreEvent, EntityDetails, EntityTabStatus } from '../../shared/entity-interface';
import { EntityDataStoreService } from '../../entity-data-store.service';
import {subscriptionHandler} from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { forkJoin, Subscription } from 'rxjs';
import {CommonService} from '../../../common/services/common.service';
import { ENTITY_VERIFICATION_STATUS, HTTP_ERROR_STATUS } from '../../../app-constants';
import { EntityManagementService } from '../../entity-management.service';
import { deepCloneObject, isEmptyObject } from '../../../common/utilities/custom-utilities';

@Component({
  selector: 'app-basic-details',
  templateUrl: './basic-details.component.html',
  styleUrls: ['./basic-details.component.scss']
})
export class BasicDetailsComponent implements OnInit, OnDestroy {
    isEditMode = false;
    @Input() sectionName: any;
    @Input() sectionId: any;
    $subscriptions: Subscription[] = [];
    entityDetails: EntityDetails = new EntityDetails();
    autoSaveEntityDetails : EntityDetails = new EntityDetails();
    entityTabStatus: EntityTabStatus = new EntityTabStatus();

    constructor(public dataStore: EntityDataStoreService, private _commonService: CommonService, private _entityManagementService: EntityManagementService) {}

    ngOnInit() {
        this.getDataFromStore(true);
        this.listenDataChangeFromStore();
    }

    private getDataFromStore(canUpdateEntity: boolean = false) {
        const entityData = this.dataStore.getData();
        if (isEmptyObject(entityData)) { return; }
        this.entityDetails = entityData.entityDetails;
        this.entityTabStatus = entityData?.entityTabStatus;
        this.checkUserHasRight();
        if(canUpdateEntity) {
            this.autoSaveEntityDetails = deepCloneObject(this.entityDetails);
        }
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: DataStoreEvent) => {
                this.getDataFromStore(dependencies.action === 'REFRESH');
            })
        );
    }

    updateStoreData(event) {
        if (!isEmptyObject(event?.autoSaveRO)) {
            Object.keys(event?.autoSaveRO).forEach((ele) => {
                this.entityDetails[ele] = event?.autoSaveRO[ele];
            });
            this.dataStore.updateStore(['entityDetails'], { 'entityDetails':  this.entityDetails });
            this.updateSponsorOrgFeed(this.entityDetails?.entityId, event?.autoSaveRO);
        }
        this.dataStore.enableModificationHistoryTracking();
    }
    updateSponsorOrgFeed(entityId, reqObj) {
        const FEED_API_CALLS = this.dataStore.getApiCalls(entityId, reqObj);
        if (FEED_API_CALLS.length && this.entityDetails.entityStatusType.entityStatusTypeCode == ENTITY_VERIFICATION_STATUS.VERIFIED) {
            this.$subscriptions.push(forkJoin(FEED_API_CALLS).subscribe((data: [] = []) => {
                    this.dataStore.updateFeedStatus(this.entityTabStatus, 'BOTH');
                }, err => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating feed status.');
                }
            ));
        }
    }
    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    checkUserHasRight(): void {
        this.isEditMode = this.dataStore.getEditMode() && this._commonService.getAvailableRight(['MANAGE_ENTITY'], 'SOME');
    }
}

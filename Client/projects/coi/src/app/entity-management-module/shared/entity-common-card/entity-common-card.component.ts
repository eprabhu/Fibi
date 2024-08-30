import { Component, EventEmitter, Input, Output } from '@angular/core';
import { EntityDetailsCard } from '../entity-interface';
import { EntityManagementService } from '../../entity-management.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

@Component({
  selector: 'app-entity-common-card',
  templateUrl: './entity-common-card.component.html',
  styleUrls: ['./entity-common-card.component.scss']
})
export class EntityCommonCardComponent {

    isExpanded = false;
    @Input() isShowUseBtn = false;
    @Input()  entityDetailsObj: any;
    @Output() emitEntityDetails = new EventEmitter<any>();
    dunsResponse: any;
    $subscriptions: Subscription[] = [];
    dunsNumber: any;
    isDunsMatched: boolean;

    constructor(public entityManagementService: EntityManagementService,
        private _dataStorService: EntityDataStoreService) {}

    ngOnInit(){
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    getPercentage() {
        return ((this.entityDetailsObj?.matchQualityInformation?.confidenceCode)/10) * 100;
    }

    sendEntityDetails() {
        this.emitEntityDetails.emit(true);
    }

    private getDataFromStore() {
        const entityData = this._dataStorService.getData();
        if (isEmptyObject(entityData)) { return; }
        this.dunsNumber = entityData?.entityDetails?.dunsNumber;
        this.isDunsMatched = entityData?.entityDetails?.isDunsMatched;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStorService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

}

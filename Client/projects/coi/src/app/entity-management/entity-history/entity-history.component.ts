import { Component, Input, OnChanges } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { EntityManagementService } from '../entity-management.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { DateFormatPipeWithTimeZone } from '../../shared/pipes/custom-date.pipe';
import { fadeInOutHeight } from '../../common/utilities/animations';
import { isEmptyObject } from '../../../../../fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-entity-history',
    templateUrl: './entity-history.component.html',
    styleUrls: ['./entity-history.component.scss'],
    animations: [fadeInOutHeight]
})
export class EntityHistoryComponent implements OnChanges {

    $subscriptions: Subscription[] = [];
    entityHistoryLogs: any = {};
    @Input() entityDetails: any;
    isEmptyObject = isEmptyObject;

    constructor( private _commonService: CommonService,
                 public entityManagementService: EntityManagementService,
                 public dataFormatPipe: DateFormatPipeWithTimeZone ) { }
    
    ngOnChanges() {
        this.getEntityHistory();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    getEntityHistory() {
        this.$subscriptions.push(this.entityManagementService.entityHistory({
            entityId: this.entityDetails.entityId,
            entityNumber: this.entityDetails.entityNumber
        }).subscribe((data: any) => {
            this.updateHistoryLogs(data);
        }, _err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    sortNull() { return 0; }

    updateHistoryLogs(data: any) {
        if (data.length) {
            this.entityHistoryLogs = [];
            data.forEach((historyObj) => {
                const date = this.dataFormatPipe.transform(historyObj.updateTimestamp);
                this.entityHistoryLogs[date] = this.entityHistoryLogs[date] ? this.entityHistoryLogs[date] : [];
                this.entityHistoryLogs[date].push(historyObj);
            });
        }
    }

}

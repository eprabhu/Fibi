import { Component, OnDestroy, OnInit } from '@angular/core';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { DateFormatPipeWithTimeZone } from '../../shared/pipes/custom-date.pipe';
import { TravelDataStoreService } from '../services/travel-data-store.service';
import { TravelDisclosureService } from '../services/travel-disclosure.service';
import { TravelDisclosure } from '../travel-disclosure-interface';
import { fadeInOutHeight } from '../../common/utilities/animations';
import { isEmptyObject } from '../../../../../fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-travel-history',
    templateUrl: './travel-history.component.html',
    styleUrls: ['./travel-history.component.scss'],
    animations: [fadeInOutHeight]
})
export class TravelHistoryComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    travelDisclosure: TravelDisclosure = new TravelDisclosure();
    disclosureHistoryLogs: any = {};
    isEmptyObject = isEmptyObject;
    isReadMore: boolean[] = [];

    constructor( public service: TravelDisclosureService,
                 private _commonService: CommonService,
                 private _dataStore: TravelDataStoreService,
                 private _dataFormatPipe: DateFormatPipeWithTimeZone ) { }

    ngOnInit(): void {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    private getDataFromStore(): void {
        this.travelDisclosure = this._dataStore.getData();
        this.getTravelDisclosureHistory();
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getTravelDisclosureHistory(): void {
        this.$subscriptions.push(
            this.service.getTravelDisclosureHistory(this.travelDisclosure.travelDisclosureId)
            .subscribe((data: any) => {
                this.updateHistoryLogs(data);
            }, _err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }));
    }

    private updateHistoryLogs(data: any): void {
        if (data.length) {
            this.disclosureHistoryLogs = {};
            data.forEach((historyObj) => {
                const date = this._dataFormatPipe.transform(historyObj.updateTimestamp);
                this.disclosureHistoryLogs[date] = this.disclosureHistoryLogs[date] ? this.disclosureHistoryLogs[date] : [];
                this.disclosureHistoryLogs[date].push(historyObj);
            });
        }
    }

    sortNull(): number {
        return 0;
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}

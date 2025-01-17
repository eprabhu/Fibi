import { Component, OnInit } from '@angular/core';
import { CoiService } from '../services/coi.service';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { DataStoreService } from '../services/data-store.service';
import { DateFormatPipeWithTimeZone } from '../../shared/pipes/custom-date.pipe';
import { fadeInOutHeight } from '../../common/utilities/animations';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-history',
    templateUrl: './history.component.html',
    styleUrls: ['./history.component.scss'],
    animations: [fadeInOutHeight]
})
export class HistoryComponent implements OnInit {

    $subscriptions: Subscription[] = [];
    dependencies = ['coiDisclosure'];
    coiDisclosure: any = {};
    disclosureHistoryLogs: any = {};
    isEmptyObject = isEmptyObject;
    isReadMore: boolean[] = [];

    constructor( public _coiService: CoiService, 
                 private _commonService: CommonService, 
                 private _dataStore: DataStoreService,
                 public _dataFormatPipe: DateFormatPipeWithTimeZone ) { }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        window.scrollTo(0,0);
    }
    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore() {
        const DATA = this._dataStore.getData(this.dependencies);
        this.coiDisclosure = DATA.coiDisclosure;
        this.getDisclosureHistory();
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                if (dependencies.some((dep) => this.dependencies.includes(dep))) {
                    this.getDataFromStore();
                }
            })
        );
    }

    getDisclosureHistory() {
        this.$subscriptions.push(this._coiService.disclosureHistory(this.coiDisclosure.disclosureId).subscribe((data: any) => {
            this.updateHistoryLogs(data);
        }, _err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    sortNull() { return 0; }

    updateHistoryLogs(data: any) {
        if (data.length) {
            this.disclosureHistoryLogs = {};
            data.forEach((historyObj) => {
                const date = this._dataFormatPipe.transform(historyObj.updateTimestamp);
                this.disclosureHistoryLogs[date] = this.disclosureHistoryLogs[date] ? this.disclosureHistoryLogs[date] : [];
                this.disclosureHistoryLogs[date].push(historyObj);
            });
        }
    }

}

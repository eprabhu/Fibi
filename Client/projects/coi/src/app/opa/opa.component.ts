import {Component, OnInit} from '@angular/core';
import {FormBuilderEvent} from '../shared/form-builder-view/form-builder-interface';
import {Subject} from 'rxjs';
import {OpaService} from './services/opa.service';
import {isEmptyObject} from '../../../../fibi/src/app/common/utilities/custom-utilities';
import {DataStoreService} from './services/data-store.service';
import {CommonService} from '../common/services/common.service';
import {environment} from '../../environments/environment';

@Component({
    selector: 'app-opa',
    templateUrl: './opa.component.html',
    styleUrls: ['./opa.component.scss']
})
export class OpaComponent implements OnInit {
    isCardExpanded = true;
    formBuilderEvents = new Subject<FormBuilderEvent>();
    opaData;
    deployMap = environment.deployUrl;
    $subscriptions;

    constructor(private _opa: OpaService,
                public commonService: CommonService,
                private dataStore: DataStoreService) {
    }

    ngOnInit(): void {
        this.getDataFromStore();
    }

    triggerSave() {
        this._opa.formBuilderEvents.next({eventType: 'SAVE'});
    }

    private getDataFromStore() {
        const opaData = this.dataStore.getData();
        if (isEmptyObject(opaData)) {
            return;
        }
        this.opaData = opaData;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }


}

import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { CoiDisclosure } from '../../coi-interface';
import { DataStoreService } from '../../services/data-store.service';

declare var $: any;

@Component({
    selector: 'app-general-comments',
    templateUrl: './general-comments.component.html',
    styleUrls: ['./general-comments.component.css']
})
export class GeneralCommentsComponent implements OnInit {

    isShow = true;
    $subscriptions: Subscription[] = [];
    dependencies = ['coiDisclosure'];
    coiDisclosure: CoiDisclosure = new CoiDisclosure();

    constructor(
        private _commonService: CommonService,
        private _dataStore: DataStoreService
    ) { }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
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

    private getDataFromStore() {
        const DATA = this._dataStore.getData(this.dependencies);
        this.coiDisclosure = DATA.coiDisclosure;
    }

}

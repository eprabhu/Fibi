import { Component } from '@angular/core';
import { OverviewTabSection } from '../shared/entity-constants';
import { CommonService } from '../../common/services/common.service';
import { EntityDataStoreService } from '../entity-data-store.service';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntireEntityDetails, EntityRisk } from '../shared/entity-interface';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

@Component({
    selector: 'app-entity-overview',
    templateUrl: './entity-overview.component.html',
    styleUrls: ['./entity-overview.component.scss']
})
export class EntityOverviewComponent {

    overViewTab: any;
    coiEntity: any;
    entityRisksList: EntityRisk[] = [];
    $subscriptions: Subscription[] = [];

    constructor(public commonService: CommonService, public _dataStoreService: EntityDataStoreService) {}

    ngOnInit() {
        window.scrollTo(0, 0);
        this.overViewTab = OverviewTabSection;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getDataFromStore() {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) {
            return;
        }
        this.entityRisksList = ENTITY_DATA?.entityRisks;
    }

}

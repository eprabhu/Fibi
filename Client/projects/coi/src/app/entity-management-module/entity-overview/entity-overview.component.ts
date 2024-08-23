import { Component } from '@angular/core';
import { OverviewTabSection } from '../shared/entity-constants';
import { CommonService } from '../../common/services/common.service';
import { EntityDataStoreService } from '../entity-data-store.service';

@Component({
  selector: 'app-entity-overview',
  templateUrl: './entity-overview.component.html',
  styleUrls: ['./entity-overview.component.scss']
})
export class EntityOverviewComponent {

    overViewTab: any;
    coiEntity: any;

    constructor(public commonService: CommonService,  public dataStore: EntityDataStoreService) {}

    ngOnInit() {
        window.scrollTo(0,0);
        this.overViewTab = OverviewTabSection;
    }

}

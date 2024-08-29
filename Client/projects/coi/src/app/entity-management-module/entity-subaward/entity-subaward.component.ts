import { Component } from '@angular/core';
import { EntityDataStoreService } from '../entity-data-store.service';
import { CommonService } from '../../common/services/common.service';
import { SubawardOrganisationTab } from '../shared/entity-constants';

@Component({
  selector: 'app-entity-subaward',
  templateUrl: './entity-subaward.component.html',
  styleUrls: ['./entity-subaward.component.scss']
})
export class EntitySubawardComponent {

  overViewTab: any;

  constructor(public commonService: CommonService, public dataStore: EntityDataStoreService) { }


  ngOnInit() {
    window.scrollTo(0,0);
      this.overViewTab = SubawardOrganisationTab;
  }

}

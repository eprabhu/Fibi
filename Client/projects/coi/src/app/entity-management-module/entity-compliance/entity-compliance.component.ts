import { Component } from '@angular/core';
import { EntityDataStoreService } from '../entity-data-store.service';
import { CommonService } from '../../common/services/common.service';
import { ComplianceTab } from '../shared/entity-constants';

@Component({
  selector: 'app-entity-compliance',
  templateUrl: './entity-compliance.component.html',
  styleUrls: ['./entity-compliance.component.scss']
})
export class EntityComplianceComponent {

  overViewTab: any;

  constructor(public commonService: CommonService, public dataStore: EntityDataStoreService) { }

  ngOnInit() {
    window.scrollTo(0,0);
    this.overViewTab = ComplianceTab;
  }


}

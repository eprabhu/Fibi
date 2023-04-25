import { Component } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { slideHorizontal } from '../../../../../fibi/src/app/common/utilities/animations';
import { DataStoreService } from '../services/data-store.service';
import { RelationshipService } from './relationship.service';

@Component({
  selector: 'app-relationship',
  templateUrl: './relationship.component.html',
  styleUrls: ['./relationship.component.scss'],
  animations: [slideHorizontal],
})
export class RelationshipComponent {
  isShowRelation = false;
  proposalArray = [];
  coiStatusList = [];
  searchText: any;
  searchResult = [];
  moduleId: any;
  moduleCode: any;
  selectedProject: any;
  coiData: any;
  selectedProjectForView: any;

  constructor(private _relationShipService: RelationshipService,private _route: ActivatedRoute,
    private _dataStore: DataStoreService,public _router: Router) { }

  closePage() {
    this.isShowRelation = false;
    this.moduleCode = null;
    this.moduleId = null;
    this.loadProjectRelations();
  }

  ngOnInit() {
    this.getDataFromStore();
    this.loadProjectRelations();
  }

  getDisclosureCount(typeCode, disclosureStatus) {
    if(disclosureStatus) {
      let value = disclosureStatus.find(ele => Object.keys(ele) == typeCode);
      return value ? value[typeCode] : 0;
    }
  }

  private getDataFromStore() {
    this.coiData = this._dataStore.getData();
    console.log(this.coiData)
}
  loadProjectRelations() {
    this._relationShipService.getProjectRelations(this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureStatusCode).subscribe((data: any) => {
      this.proposalArray = data.awards;
      data.proposals.every(ele => this.proposalArray.push(ele));
      this.coiStatusList = data.coiConflictStatusTypes;
    });
  }

  openDefineRelationship(test) {
    this.moduleCode = test.moduleCode;
    this.moduleId = test.moduleItemId ;
    this.selectedProject = test;
    this.isShowRelation = true;
  }

}

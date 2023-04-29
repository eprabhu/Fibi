import { Component } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { slideHorizontal } from '../../../../../fibi/src/app/common/utilities/animations';
import { environment } from '../../../environments/environment';
import { CommonService } from '../../common/services/common.service';
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
  deployMap = environment.deployUrl;
  searchText: any;
  searchResult = [];
  moduleId: any;
  moduleCode: any;
  selectedProject: any;
  coiData: any;
  isShowWhiteIcon = false;
  selectedProjectForView: any;
  isEditMode: boolean = true;
  collapseViewMore = {};

  constructor(private _relationShipService: RelationshipService,private _route: ActivatedRoute,
    private _dataStore: DataStoreService,public _router: Router, private _commonService: CommonService) { }

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
    this.isEditMode = this.coiData.coiDisclosure.reviewStatusCode == '1';
}
  loadProjectRelations() {
    this._relationShipService.getProjectRelations(this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureStatusCode).subscribe((data: any) => {
      this.proposalArray = data.awards;
      data.proposals.every(ele => this.proposalArray.push(ele));
      this.coiStatusList = data.coiProjConflictStatusTypes;
    });
  }

  openDefineRelationship(test) {
    this.moduleCode = test.moduleCode;
    this.moduleId = test.moduleItemId ;
    this.selectedProject = test;
    this.isShowRelation = true;
  }

  collapseViewMoreOption(id: number, flag: boolean): void {
    this.collapseViewMore[id] = !flag;
  }

  openProjectMoreDetails(moduleId, moduleCode) {
    if (moduleCode == 3) {
      let test = this._commonService.fibiUrl + '#/fibi/proposal/overview?proposalId=' + moduleId;
      window.open (test, '_blank')
      // this._router.navigate([test,{ queryParams: { proposalId:  moduleId}}]);
    } else {
      let test2 = this._commonService.fibiUrl + '#/fibi/award/overview?awardId=' + moduleId;
      window.open (test2, '_blank')
      // this._router.navigate([test2,{ queryParams: { awardId:  moduleId}}]);
    }
  }
}

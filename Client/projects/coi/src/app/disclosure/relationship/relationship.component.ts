import { Component,Input, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { slideHorizontal } from '../../../../../fibi/src/app/common/utilities/animations';
import { environment } from '../../../environments/environment';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { DataStoreService } from '../services/data-store.service';
import { RelationshipService } from './relationship.service';

@Component({
  selector: 'app-relationship',
  templateUrl: './relationship.component.html',
  styleUrls: ['./relationship.component.scss'],
  animations: [slideHorizontal],
})
export class RelationshipComponent implements OnInit {
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
  expandInfo = true;
  
  constructor(private _relationShipService: RelationshipService,private _route: ActivatedRoute,
    private _dataStore: DataStoreService,public _router: Router, private _commonService: CommonService) { }

  closePage() {
    this.isShowRelation = false;
    this.moduleCode = null;
    this.moduleId = null;
    if(this.isEditMode) {
      this.updateConflictStatus();
      this.loadProjectRelations(); 
    }
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

  private updateConflictStatus(): void {
    this._relationShipService.updateConflictStatus(this.coiData.coiDisclosure.disclosureId).subscribe((data: any) => {
      if(data) {
        this.coiData.coiDisclosure.coiConflictStatusType = data;
        this.coiData.coiDisclosure.conflictStatusCode = data.conflictStatusCode;
        this._dataStore.updateStore(['coiDisclosure'],  this.coiData);
      }
    }, err => {
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating status');
    });
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

  /**
   * No Conflict - 1
   * Potential Conflict - 2
   * conflict - 3
   */
  getDisclosureStatus(): any {
   let test : any;
    this.proposalArray.forEach(ele => {
         test = ele.disclosureStatusCount.find(ele => ele[3] > 0) ? '3' :
                ele.disclosureStatusCount.find(ele => ele[2] > 0) ? '2' :
                ele.disclosureStatusCount.find(ele => ele[1] > 0) ? '1' : null;
    });
    return test;
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
      const redirectToProposal = this._commonService.fibiApplicationUrl + '#/fibi/proposal/overview?proposalId=' + moduleId;
      window.open (redirectToProposal);
      // this._router.navigate([test,{ queryParams: { proposalId:  moduleId}}]);
    } else {
      const redirectToAward = this._commonService.fibiApplicationUrl + '#/fibi/award/overview?awardId=' + moduleId;
      window.open (redirectToAward);
      // this._router.navigate([test2,{ queryParams: { awardId:  moduleId}}]);
    }
  }
}

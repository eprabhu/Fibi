// last updated by Aravind on 08-03-2021

import { Component, OnInit, Input, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';


import { GrantCallService } from '../../services/grant.service';
import { AreaOfResearchService } from './area-of-research.service';
import { GrantCommonDataService } from '../../services/grant-common-data.service';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { areaOfResearch } from '../../grant-call-interfaces';

@Component({
  selector: 'app-area-of-research',
  templateUrl: './area-of-research.component.html',
  styleUrls: ['./area-of-research.component.css']
})
export class AreaOfResearchComponent implements OnInit, OnDestroy {

  areaHttpOptions: any = {};
  subAreaHttpOptions: any = {};
  fundingAgencyHttpOptions: any = {};

  selectedSubArea: any = null;
  selectedArea: any = null;
  completerOptions: any = {};
  @Input() result: any = {};
  @Input() mode: any;
  showDataFlagObj: any = {};
  warningMsgObj: any = {};
  isAreaError = false;
  clearAreaField: String;
  clearSubAreaField: String;
  removeObjId: any;
  researchAreaId: any;
  deleteIndex: any;
  $subscriptions: Subscription[] = [];
  isShowAreaOfResearch = false;
  selectedResearchTypeCode: any;
  areaOfResearchEditObject: areaOfResearch;
  editIndex: number ;
  isEditAreaOfResearch = false;

  constructor(private _grantService: GrantCallService, private _areaResearchService: AreaOfResearchService,
    private _commonData: GrantCommonDataService, public _commonService: CommonService) { }

  ngOnInit(): void {
    this.isShowAreaOfResearch = true;
    this.selectedResearchTypeCode = this.result.researchTypes === null ?
      null : this.researchTypeSet();
    this.setAreaOptions('');
    this.setSubAreaOptions('');
  }

  ngOnDestroy(): void {
    subscriptionHandler(this.$subscriptions);
  }

  /** Check whether any research type is active or not */
  researchTypeSet(): number {
    const researchTypeCode = this.result.researchTypes.find(type => type.isActive === true);
    this.selectedResearchTypeCode = researchTypeCode ? researchTypeCode.researchTypeCode : null;
    return this.selectedResearchTypeCode;
  }

  /* changes research type */
  researchTypeChange(): void {
    this.warningMsgObj.researchWarningMsg = null;
    this.isAreaError = false;
    this.setAreaOptions();
    this.setSubAreaOptions('');
  }

  setAreaOptions(defaultValue = ''): void {
    this.areaHttpOptions = this._grantService.setEndPointSearchOptions('description', 'description',
      'findResearchTypeArea', defaultValue, { 'researchTypeCode': this.selectedResearchTypeCode });
  }

  /** sets end point search options for sub-area with dynamic params based on conditions
   * @param defaultValue
   */
  setSubAreaOptions(defaultValue): void {
    const SUB_AREA_PARAM = {
      'researchTypeCode': this.selectedResearchTypeCode,
      'researchTypeAreaCode': this.selectedArea ? this.selectedArea.researchTypeAreaCode : null
    };
    this.subAreaHttpOptions =
      this._grantService.setEndPointSearchOptions('description', 'description', 'findResearchTypeSubArea',
        defaultValue, SUB_AREA_PARAM);
  }

  /** sets area object from end point search, also clears sub-area field
   * @param result
   */
  researchAreaSelectedFunction(result): void {
    this.selectedArea = result ? result : null;
    this.selectedSubArea = null;
    this.setSubAreaOptions('');
  }

  /** sets sub-area object from end point search, also defaults the value of area
   * @param result
   */
  researchSubAreaSelectedFunction(result): void {
    if (result) {
      this.selectedSubArea = result;
      if (result.researchTypeArea && result.researchTypeArea.description) {
        this.selectedArea = result.researchTypeArea;
        this.areaHttpOptions.defaultValue = result.researchTypeArea.description;
        this.isAreaError = false;
        this.clearAreaField = new String('false');
      }
    } else {
      this.selectedSubArea = null;
    }
  }

  /** function validates areas according to type chosen and calls method to add if validation is successful */
  validateAndSetReqObject(): void {
    this.validateAreaOfResearch();
    if (!this.warningMsgObj.researchWarningMsg) {
      this.setResearchAreaReqObject();
      this.clearResearchArea();
      this.clearResearchSubArea();
      this.setSubAreaOptions('');
    }
  }

  /** main method to validate area of research */
  validateAreaOfResearch(): void {
    this.warningMsgObj.researchWarningMsg = null;
    if (this.selectedArea != null && this.result.grantCallResearchAreas.length !== 0) {
      this.validateResearchArea();
    } else if (!this.selectedArea) {
      this.isAreaError = true;
      this.warningMsgObj.researchWarningMsg = '* Please add Research Area';
    }
  }

  /** method to validate research area */
  validateResearchArea(): void {
    for (const area of this.result.grantCallResearchAreas) {
      if (!this.isEditMode(area)) {
        if ((area.researchTypeCode == this.selectedResearchTypeCode) &&
          (area.researchTypeAreaCode == this.selectedArea.researchTypeAreaCode) &&
          ((!area.researchTypeSubArea && !this.selectedSubArea) ||
            (this.selectedSubArea && area.researchTypeSubAreaCode &&
              area.researchTypeSubAreaCode == this.selectedSubArea.researchTypeSubAreaCode))) {
          this.warningMsgObj.researchWarningMsg = 'Area already added';
          break;
        }
      }
    }
  }


  isEditMode(researchArea) {
    if (this.isEditAreaOfResearch) {
      if (!researchArea.researchAreaId || researchArea.researchAreaId == this.areaOfResearchEditObject.grantResearchAreaId) {
        return true;
      }
    }
    return false;
  }

  /** sets request object for research area */
  setResearchAreaReqObject(): void {
    const areaObject: any = {};
    areaObject.researchTypeCode = this.selectedResearchTypeCode;
    areaObject.researchType = this.result.researchTypes
      .find(area => area.researchTypeCode === this.selectedResearchTypeCode);
    areaObject.researchTypeAreaCode = this.selectedArea.researchTypeAreaCode;
    areaObject.researchTypeArea = this.selectedArea;
    areaObject.researchTypeSubAreaCode = this.selectedSubArea == null ? null : this.selectedSubArea.researchTypeSubAreaCode;
    areaObject.researchTypeSubArea = this.selectedSubArea == null ? null : this.selectedSubArea;
    if (this.isEditAreaOfResearch) {
    areaObject.grantResearchAreaId = this.areaOfResearchEditObject.grantResearchAreaId;
    }
    this.addAreaOfResearch(areaObject);
  }

  /** common request object for area of research and adds them
   * @param areaOfResearchEditObject
   */
  addAreaOfResearch(areaOfResearchEditObject: areaOfResearch): void {
    areaOfResearchEditObject.updateTimeStamp = new Date().getTime();
    areaOfResearchEditObject.updateUser = this._commonService.getCurrentUserDetail('userName');
    areaOfResearchEditObject.grantCallId = this.result.grantCall.grantCallId;
    this.$subscriptions.push(this._areaResearchService.addGrantCallAreaOfResearch({
      'grantCallId': this.result.grantCall.grantCallId,
      'updateUser': this._commonService.getCurrentUserDetail('userName'),
      'grantCallResearchArea': areaOfResearchEditObject
    }).subscribe(newResearchAreaDetails => {
      this.result.grantCallResearchAreas = newResearchAreaDetails;
      this.updateGrantCallStoreData();
      this.clearResearchArea();
    },
      err => { this.errorToastWhileAddingResearchArea(); },
      () => { this.successToastWhileAddingResearchArea(); }));
  }


  errorToastWhileAddingResearchArea(): void {
    this._commonService.showToast(HTTP_ERROR_STATUS, 'Adding Area failed. Please try again.');
  }

  successToastWhileAddingResearchArea(): void {
    if (this.isEditAreaOfResearch) {
      this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Area updated successfully.');
    } else {
      this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Area added successfully.');
    }
    this.isEditAreaOfResearch = false;
  }

  /** clears area field */
  clearResearchArea(): void {
    this.selectedArea = null;
    this.isAreaError = false;
    this.areaHttpOptions.defaultValue = '';
    this.clearAreaField = new String('true');
  }

  /** clears sub-area field*/
  clearResearchSubArea(): void {
    this.selectedSubArea = null;
    this.subAreaHttpOptions.defaultValue = '';
    this.clearSubAreaField = new String('true');
  }
  setAreaOfResearchObject(grantResearchAreaId, index): void {
    this.researchAreaId = grantResearchAreaId;
    this.deleteIndex = index;
  }

  deleteAreaOfResearch(): void {
    this.$subscriptions.push(this._areaResearchService.deleteGrantCallAreaOfResearch({
      'grantCallId': this.result.grantCall.grantCallId,
      'grantResearchAreaId': this.researchAreaId
    }).subscribe(data => {
      this.result.grantCallResearchAreas.splice(this.deleteIndex, 1);
      this.updateGrantCallStoreData();
      this.resetAreaOfResearch();
    },
      err => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Removing Area failed. Please try again.');
      },
      () => {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Area removed successfully.');
      }));
  }
  /**
  * setup grant call common data the values that changed after the service call need to be updatedinto the store.
  * every service call wont have all the all the details as response so
  * we need to cherry pick the changes and update them to the store.
  */
  updateGrantCallStoreData(): void {
    this.result = JSON.parse(JSON.stringify(this.result));
    this._commonData.setGrantCallData(this.result);
  }

  editAreaOfResearch(index) {
    this.warningMsgObj.researchWarningMsg = null;
    this.isAreaError = false;
    this.isEditAreaOfResearch = true;
    this.editIndex = index;
    this.areaOfResearchEditObject = JSON.parse(JSON.stringify(this.result.grantCallResearchAreas[index]));
    this.selectedResearchTypeCode = this.areaOfResearchEditObject.researchTypeCode;
    this.clearSubAreaField = new String('false');
    this.clearAreaField = new String('false');
    this.selectedArea =  this.areaOfResearchEditObject.researchTypeArea;
    this.selectedSubArea = this.areaOfResearchEditObject.researchTypeSubArea;
    this.setAreaOptions( this.areaOfResearchEditObject.researchTypeArea.description);
    let subAreaDescription=this.areaOfResearchEditObject.researchTypeSubArea ? this.areaOfResearchEditObject.researchTypeSubArea.description : '';
    this.setSubAreaOptions(subAreaDescription);
  }

  resetAreaOfResearch() {
    this.warningMsgObj.researchWarningMsg = null;
    this.isAreaError = false;
    this.isEditAreaOfResearch = false;
    this.clearResearchArea();
    this.clearResearchSubArea();

  }
}

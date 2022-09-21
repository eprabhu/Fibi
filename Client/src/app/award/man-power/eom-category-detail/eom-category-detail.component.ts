import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { Observable, Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { setFocusToElement } from '../../../common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { CommonDataService } from '../../services/common-data.service';
import { ManPowerService } from '../man-power.service';
import { calculateUncommittedAmount, compareSequenceNumber } from '../manpower-utilities';

@Component({
  selector: 'app-eom-category-detail',
  templateUrl: './eom-category-detail.component.html',
  styleUrls: ['./eom-category-detail.component.css']
})
export class EomCategoryDetailComponent implements OnInit, OnDestroy, OnChanges {

  @Input() manpowerCategory: any;
  @Input() helpText: any;
  @Input() awardData: any;
  @Input() isManpowerEdit: boolean;
  @Input() isShowAllDetails: any = [];
  @Input() emitChildResponse: Observable<any>;
  @Input() componentIndex: string;
  @Output() resourceOperations: EventEmitter<any> = new EventEmitter<any>();
  setFocusToElement = setFocusToElement;
  $subscriptions: Subscription[] = [];
  canViewPayrollDetails = false;
  canTriggerPosition = false;
  compareSequenceNumber = compareSequenceNumber;
  canEditCommittedAmount = false;

  constructor(private _manpowerService: ManPowerService, private _commonData: CommonDataService, public _commonService: CommonService) { }

  ngOnInit() {
    this.manpowerCategory.uncommittedAmount = calculateUncommittedAmount(this.manpowerCategory.awardManpowerResource,
      this.manpowerCategory.budgetAmount, this.manpowerCategory.sapCommittedAmount);
    this.updateUserDetailsFromParent();
  }

  ngOnChanges() {
    this.getPermissions();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  getPermissions() {
    this.canViewPayrollDetails = this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_VIEW_STAFF_PAYROLL');
    this.canTriggerPosition = this.awardData.award.awardDocumentTypeCode != '1' && this._commonData.getSectionEditableFlag('133') &&
    this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_ENABLE_TRIGGER_POSITION');
    this.canEditCommittedAmount = this._commonData.getSectionEditableFlag('131') &&
      this._commonData.checkDepartmentLevelRightsInArray('MANPOWER_MODIFY_STAFF_COMMITTED_AMOUNT');
  }
  /**
   * for triggering the resource when the trigger me is clicked
   */
  triggerResource(resource, awardManpowerId) {
    this.resourceOperations.emit({ 'resource': resource, 'operationType': 'triggerResource', 'awardManpowerId': awardManpowerId });
  }
  /**
   * add or update resource details. This function emits the data to the main component where the service is called
   */
  addResource(index = null, type: string, resource = null) {
    this.resourceOperations.emit({
      'operationType': 'addResource', 'categoryType': 'Staff', 'index': index,
      'addStaffType': type ? type : resource.personId ? 'Existing' : 'New',
      'componentIndex': this.componentIndex, 'manpowerCategory': this.manpowerCategory, 'resourceObject': resource
    });
  }
  /**
   * @param  {} code
   * for setting the badge colour
   * 1 -	Position Not Triggered
   * 2 -	Ready To Trigger Position
   * 3 -	Position Triggering
   * 4 -	Active
   * 5 -	Ended
   */
  getPositionStatusClass(code) {
    if (code === '1') {
      return 'warning';
    } else if (code === '4') {
      return 'success';
    } else if (code === '5' || code === '8') {
      return 'danger';
    } else {
      return 'info';
    }
  }
  /**
   * @param  {} resourceModalData
   * for setting the data for showing the information for the person modal
   */
  showDetailsModal(resourceModalData) {
    this.resourceOperations.emit({
      'resourceDetails': resourceModalData, 'operationType': 'personDetailsModal',
      'category': 'Staff', 'manpowerType': this.manpowerCategory.manpowerType
    });
  }
  /**
   * @param  {} resourceId
   * @param  {} category
   * @param  {} index
   * emit the data to parent component to perform deletion
   */
  deleteResource(resourceId, category, index) {
    this._manpowerService.manpowerCategory = category;
    this.resourceOperations.emit({
      'deleteResourceId': resourceId, 'operationType': 'deleteResource',
      'index': index, 'componentIndex': this.componentIndex, 'awardManpowerId': this.manpowerCategory.awardManpowerId
    });
  }
  /**
   * @param  {string} personId
   * @param  {string} ioCode
   * @param  {string} name
   * emit the data to parent component to trigger the payroll modal
   */
  payrollModal(personId: string, ioCode: string, name: string) {
    this.resourceOperations.emit({ 'personId': personId, 'ioCode': ioCode, 'operationType': 'payrollModal', 'name': name });
  }

  updateUserDetailsFromParent() {
    this.$subscriptions.push(this.emitChildResponse.subscribe(data => {
      if (data && data.childComponentIndex === this.componentIndex
        && ['editActualCommitted', 'deleteResource'].includes(data.action)) {
        this.manpowerCategory.uncommittedAmount = calculateUncommittedAmount(this.manpowerCategory.awardManpowerResource,
          this.manpowerCategory.budgetAmount, this.manpowerCategory.sapCommittedAmount);
      }
    }));
  }

   editActualCommitted(resource, category, index) {
    this.resourceOperations.emit({ 'resource': resource, 'category': category,
    'operationType': 'editActualCommitted', 'index': index, 'childComponentIndex': this.componentIndex });
  }

}

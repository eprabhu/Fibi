import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { ManPowerService } from '../man-power.service';

@Component({
  selector: 'app-other-category-details',
  templateUrl: './other-category-details.component.html',
  styleUrls: ['./other-category-details.component.css']
})
export class OtherCategoryDetailsComponent implements OnInit, OnDestroy {

  @Input() manpowerCategory: any;
  @Input() helpText: any;
  @Input() awardData: any;
  @Input() isManpowerEdit: boolean;
  @Output() resourceOperations: EventEmitter<any> = new EventEmitter<any>();
  $subscriptions: Subscription[] = [];

  constructor(public _commonService: CommonService, private _manpowerService: ManPowerService) { }

  ngOnInit() {}

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }
  /**
   * add or update resource details. This function emits the data to the main component where the service is called
   */
  addResource(index = null, resource = null) {
    this.resourceOperations.emit({'operationType': 'addResource', 'categoryType': 'Others', 'index': index,
      'addStaffType': null, 'manpowerCategory': this.manpowerCategory, 'resourceObject': resource});
  }
  /**
   * @param  {} resourceId
   * @param  {} category
   * @param  {} index
   * emit the data to parent component to perform deletion
   */
  deleteResource(resourceId, category, index) {
    this._manpowerService.manpowerCategory = category;
    this.resourceOperations.emit({ 'deleteResourceId': resourceId, 'operationType': 'deleteResource', 'index': index});
  }
  /**
   * @param  {} resourceModalData
   * for setting the data for showing the information for the person modal
   */
  showDetailsModal(resourceModalData) {
    this.resourceOperations.emit({'resourceDetails': resourceModalData, 'operationType': 'personDetailsModal',
    'category': 'Others', 'manpowerType': this.manpowerCategory.manpowerType});
  }

}

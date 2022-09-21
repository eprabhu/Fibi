import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { CommonDataService } from '../../services/common-data.service';
import { ManPowerService } from '../man-power.service';

@Component({
  selector: 'app-rss-category-details',
  templateUrl: './rss-category-details.component.html',
  styleUrls: ['./rss-category-details.component.css']
})
export class RssCategoryDetailsComponent implements OnInit, OnDestroy {

  @Input() manpowerCategory: any;
  @Input() helpText: any;
  @Input() awardData: any;
  @Input() isManpowerEdit: boolean;
  @Input() isShowAllDetails: any = [];
  @Input() componentIndex: number;
  @Output() resourceOperations: EventEmitter<any> = new EventEmitter<any>();
  $subscriptions: Subscription[] = [];

  constructor(private _manpowerService: ManPowerService, public _commonService: CommonService, private _commonData: CommonDataService) { }

  ngOnInit() {}

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }
  /**
   * add or update resource details. This function emits the data to the main component where the service is called
   */
  addResource(index = null, resource = null) {
    this.resourceOperations.emit({'operationType': 'addResource', 'categoryType': 'Student', 'index': index,
      'addStaffType': null, 'manpowerCategory': this.manpowerCategory, 'resourceObject': resource});
  }
  /**
   * @param  {} resourceModalData
   * for setting the data for showing the information for the person modal
   */
  showDetailsModal(resourceModalData) {
    this.resourceOperations.emit({'resourceDetails': resourceModalData, 'operationType': 'personDetailsModal',
    'category': 'Student', 'manpowerType': this.manpowerCategory.manpowerType});
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

}

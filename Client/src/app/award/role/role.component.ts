import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonDataService } from '../services/common-data.service';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';

@Component({
  selector: 'app-role',
  templateUrl: './role.component.html',
  styleUrls: ['./role.component.css']
})
export class RoleComponent implements OnInit, OnDestroy {

  isRoleEdit = false;
  $subscriptions: Subscription[] = [];

  constructor(public _commonData: CommonDataService) { }

  ngOnInit() {
    this.getAwardGeneralData();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  getAwardGeneralData() {
    this.$subscriptions.push(this._commonData.awardData.subscribe((data: any) => {
      if (data) {
        this.isRoleEdit = (this._commonData.getSectionEditableFlag('107') ||
        this._commonData.checkDepartmentLevelRightsInArray('MODIFY_DOCUMENT_PERMISSION')) &&
        data.award.awardSequenceStatus !== 'ARCHIVE' && data.award.awardSequenceStatus !== 'CANCELLED';
      }
    }));
  }

}

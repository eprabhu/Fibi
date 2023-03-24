import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonDataService } from '../services/common-data.service';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';

@Component({
  selector: 'app-reporting-requirements',
  templateUrl: './reporting-requirements.component.html',
  styleUrls: ['./reporting-requirements.component.css']
})
export class ReportingRequirementsComponent implements OnInit, OnDestroy {

  isReportsEdit = false;
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
        this.isReportsEdit = this._commonData.getSectionEditableFlag('109');
      }
    }));
  }
}

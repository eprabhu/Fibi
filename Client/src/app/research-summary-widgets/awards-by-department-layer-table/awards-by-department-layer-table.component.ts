import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { fadeDown } from '../../common/utilities/animations';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { ResearchSummaryWidgetsService } from '../research-summary-widgets.service';

@Component({
  selector: 'app-awards-by-department-layer-table',
  templateUrl: './awards-by-department-layer-table.component.html',
  styleUrls: ['./awards-by-department-layer-table.component.css'],
  animations: [fadeDown]
})
export class AwardsByDepartmentLayerTableComponent implements OnInit, OnDestroy {

  $subscriptions: Subscription[] = [];
  tableData: any = [];
  tableHeaderData: any = [];
  isShowLoader = false;
  breadCrumbs: any = [[
    '', , , , , , , 0, ,
  ]];
  widgetDescription: any;
  column: number;
  direction: number = -1;
  isDesc: boolean;

  constructor(private _researchSummaryWidgetService: ResearchSummaryWidgetsService) { }

  ngOnInit() {
    this.widgetDescription = this._researchSummaryWidgetService.getWidgetDescription(17);
    this.getAwardsByDepartment();
  }

  getAwardsByDepartment(level = null, unitData = null, unitNumber = null) {
    this.isShowLoader = true;
    this.$subscriptions.push(this._researchSummaryWidgetService
      .getResearchSummaryDatasByWidget({ tabName: 'AWARDS_BY_DEPARTMENT', level: level, unitNumber: unitNumber})
      .subscribe((data: any) => {
        this.isShowLoader = false;
        this.tableData = data.widgetDatas;
        this.tableHeaderData = this.tableData.splice(0, 1)[0];
      },
        err => { this.isShowLoader = false; }));
    if (level && unitData) {
      this.breadCrumbs.push(unitData);
    }
  }
  /**
   * @param  {} data
   * breadcrumbs are created using an array which is would display as list items and
   * is navigated by pushing and slicing the array
   */
  setBreadCrumbs(data, index) {
    this.breadCrumbs = this.breadCrumbs.slice(0, index + 1);
    const PREVIOUS_LEVEL = parseInt(data[8], 10);
    this.getAwardsByDepartment(PREVIOUS_LEVEL, null, this.breadCrumbs[index][1]);
  }

sortBy(property) {
    this.column = property;
    this.direction = this.isDesc ? 1 : -1;
}


  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

}

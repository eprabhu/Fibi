  /**
     * sectionType = S (Summary of Progress)
     * sectionType = F (Future Plans)
     */
import { Component, OnInit, Input } from '@angular/core';
import { CommonDataService } from './../../services/common-data.service';
import { ProgressReportService } from './../../services/progress-report.service';
import { CommonService } from './../../../common/services/common.service';


@Component({
  selector: 'app-future-plan-summary',
  templateUrl: './future-plan-summary.component.html',
  styleUrls: ['./future-plan-summary.component.css']
})
export class FuturePlanSummaryComponent{

  @Input() futurePlansNSummaryData: any = [];
  @Input() isEditMode: boolean;
  @Input() sectionType: any;
  @Input() validationMap;
  @Input() reportClassCode;
  constructor(public _commonData: CommonDataService,  public _ProgressReportService: ProgressReportService, public commonService: CommonService ) { }
}


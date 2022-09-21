import { Component, Input, OnInit } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';

@Component({
  selector: 'app-manpower-person-details',
  templateUrl: './manpower-person-details.component.html',
  styleUrls: ['./manpower-person-details.component.css']
})
export class ManpowerPersonDetailsComponent implements OnInit {

  @Input() resourceModalDetails: any;

  constructor(public _commonService: CommonService) { }

  ngOnInit() {
  }
  /**
   * clearing the modal data on close
   */
  closeModal() {
    this.resourceModalDetails = {};
  }

}

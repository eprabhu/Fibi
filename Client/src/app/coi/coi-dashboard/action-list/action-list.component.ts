import { Component, OnInit } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';

@Component({
  selector: 'app-action-list',
  templateUrl: './action-list.component.html',
  styleUrls: ['./action-list.component.css']
})
export class ActionListComponent implements OnInit {

  constructor(public commonService: CommonService) { }

  ngOnInit() {
  }

}

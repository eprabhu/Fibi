import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-additional-sfi-details',
  templateUrl: './additional-sfi-details.component.html',
  styleUrls: ['./additional-sfi-details.component.scss']
})
export class AdditionalSfiDetailsComponent implements OnInit {

  constructor() { }
  sponsorsResearch = 'Y';
  isExpanded = true;
  ngOnInit() {
  }
  alert() {
    console.log(this.isExpanded)
  }
}

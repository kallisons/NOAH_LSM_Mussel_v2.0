BEGIN{readflag=0;count=0;}
{count++;
begin_string="   State Variables at the end";
if ($0 == begin_string){readflag=1;count=0;}
if((readflag==1)&&(count>3)&&(count<=12)) print $0
}


/*  
 *  Description: Class to store properties about staff members 
 *  and calculate Gross and Net pay.
*/
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Coursework
{
    public class Staff
    {
        public Staff() { }

        //  4 Bit staff ID of staff member between 1000 - 2000
        private int staffId;
        public int StaffId
        {
            get
            {
                return staffId;
            }
            set
            {
                if (value < 1000 || value > 2000)
                {
                    throw new Exception("Staff ID must be between 1000 and 2000");
                }
                else
                {
                    staffId = value;
                }
            }
        }

        //  First name of staff member
        private string firstName;
        public string FirstName
        {
            get
            {
                return firstName;
            }
            set
            {
                firstName = checkString(value);
            }
        }

        //  Second name of staff member
        private string secondName;
        public string SecondName
        {
            get
            {
                return secondName;
            }
            set
            {
                secondName = checkString(value);
            }
        }

        //  Date of birth of staff member
        private DateTime dateOfBirth;
        public DateTime DateOfBirth
        {
            get
            {
                return dateOfBirth;
            }
            set
            {
                checkString(value.ToString());
                dateOfBirth = value;
            }
        }

        //  Department the staff member works for
        private string department;
        public string Department
        {
            get
            {
                return department;
            }
            set
            {
                department = checkString(value);
            }
        }

        //  Hourly pay rate of staff member stored as pence
        private float hourlyPayRate;
        public float HourlyPayRate
        {
            get
            {
                return hourlyPayRate;
            }
            set
            {
                if (value < 0 || value > 99999)
                {
                    throw new Exception("Pay rate must be between £0 and £999.99");
                }
                else
                {
                    hourlyPayRate = value * 100;
                }
            }
        }

        //  Weekly hours worked by staff member
        private float hoursWorked;
        public float HoursWorked
        {
            get
            {
                return hoursWorked;
            }
            set
            {
                if (value < 1 || value > 65)
                {
                    throw new Exception("Hours worked must be between 0 and 65");
                }
                else
                {
                    hoursWorked = value;
                }
            }
        }

        //  Gross pay of staff member
        private float grossPay;
        public float GrossPay
        {
            get
            {
                return grossPay;
            }
            set
            {
                grossPay = value;
            }
        }

        //  Method to check string is not empty, used for data validation, throws exception if empty
        public string checkString(string stringToCheck)
        {
            if (stringToCheck == "")
            {
                throw new Exception("Text boxes must not be empty");
            }
            else
            {
                return stringToCheck;
            }
        }

        //  Method to calculate net pay
        public float calcPay(int hours)
        {
            return ((hourlyPayRate * hours) * ((100 - calcTaxRate((int)Math.Ceiling(grossPay))) / 100)) / 100;
        }

        // Method to calculate tax rate based on gross pay
        public virtual float calcTaxRate(int grossPayPence)
        {
            if (grossPayPence < 100000)
            {
                return 10;
            }
            else
            {
                return 20;
            }
        }
    }
}
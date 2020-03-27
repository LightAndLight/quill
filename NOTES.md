# Notes

## Dependencies

* tracker -> zlib

## Database

```
type AUD = { dollars : Int, cents : Int }

-- generates `type Expenses = Many { id: Int, name: Text, amount: AUD }`
-- `type Expenses.row = { id: Int, name: text, amount: AUD }`
table Expenses {
  id: Int, PK(id), AUTO_INCREMENT(id),
  name: Text,
  amount: AUD
}

query insertExpense(name: Text, amount: AUD) : Query { name: Text, amount: AUD } {
  expense <- insert { name, amount } into Expenses;
  return expense;
}

query selectExpenseById(id: Int) : Query (Many { name: Text, amount: AUD }) {
  expenses <- select from Expenses;
  return
    (for expense in expenses
     where (expense.id == id)
     yield { name: expense.name, amount: expense.amount };
    )
}

fn selectExpenseById(expenses: Expenses, id: Int) -> Many { name: Text, amount: AUD } {
  for expense in expenses
  where (expense.id == id)
  yield { name: expense.name, amount: expense.amount }
}
```
